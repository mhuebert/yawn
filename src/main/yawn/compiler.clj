(ns yawn.compiler
  "
  Hicada - Hiccup compiler aus dem Allgaeu

  NOTE: The code for has been forked like this:
  weavejester/hiccup -> r0man/sablono -> Hicada."
  (:refer-clojure :exclude [compile])
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   [cljs.analyzer :as ana]
   [yawn.wrap-return :refer [wrap-return]]
   [yawn.convert :as convert]
   [yawn.infer :as infer]
   [yawn.util :as util]
   [yawn.convert :as convert]
   [yawn.shared :as shared]
   yawn.react))

(def ^:dynamic *env* nil)

(defn children-as-list
  "Normalize the children of a HTML element."
  [x]
  (cond (nil? x) x
        (vector? x) (list x)
        (sequential? x) x
        :else (list x)))

(defn props-mode [props]
  (or (when (map? props) :map)
      (when (nil? props) :nil)
      (when (or (seq? props) (symbol? props))
        (let [props-meta (meta props)
              tag (or (:tag props-meta) (some->> *env*
                                                 (infer/infer-type props)))]
          (cond (or (:props props-meta) (= 'props tag)) :dynamic
                (#{'object 'js} tag) :js-object)))
      :no-props))

(defn analyze-vec
  "Given:
  [:div.x.y#id (other)]
  Returns:
  [:div {:id \"id\"
         :class [\"x\" \"y\"]}
    (other)]"
  [[tag & body :as vec]]
  (let [[tag id class-string] (if (or (keyword? tag)
                                      (string? tag))
                                (convert/parse-tag (name tag))
                                [tag nil nil])
        tag-override (convert/custom-elements tag)
        is-element? (= "createElement" tag-override)
        create-element? (or (some? tag-override)
                            is-element?
                            (string? tag)
                            (keyword? tag)
                            ('js (:tag (meta tag)))
                            (:el (meta tag)))]
    (if create-element?
      (let [[tag body] (if is-element? [(first body) (rest body)]
                                       [(or tag-override tag) body])
            props (first body)
            mode (props-mode props)
            props? (not= mode :no-props)]
        [tag
         (when props? props)
         (children-as-list (cond-> body props? next))
         (merge
          {:create-element? true
           :id id
           :class-string class-string
           :prop-mode mode}
          (select-keys (meta vec) [:ref :key]))])
      [tag nil body {:form-meta (meta vec)}])))


(comment
 (analyze-vec [:div#foo 'a])
 (analyze-vec [:div.a#foo])
 (analyze-vec [:h1.b {:className "a"}])
 (analyze-vec '[:div (for [x xs] [:span 1])]))

(defn compile-mode
  [form]
  (if (util/primitive? form)
    :el
    (let [{:keys [el interpret tag]} (meta form)]
      (cond (= tag 'js) :el
            el :el
            interpret :interpret
            (vector? form) :compile
            :else :maybe-interpret))))

(defmacro maybe-interpret-class [s]
  (if (= 'string (infer/infer-type s &env))
    s
    (do
      (shared/warn-on-interpret s)
      `(~'yawn.convert/class-string ~s))))

(declare emit literal->js)

(defn literal->js
  "Efficiently emit to literal JS form"
  [x]
  (cond
    (nil? x) x
    (keyword? x) (name x)
    (string? x) x
    (vector? x) (apply list 'cljs.core/array (mapv literal->js x))
    (map? x) (when (seq x)
               (assert (every? util/primitive? (keys x)))
               `(~'js-obj ~@(->> x (apply concat) (map literal->js))))
    :else x))

(defn join-strings-compile [v]
  (cond (string? v) v
        (coll? v)
        (let [v (interpose " " v)
              v (->> (partition-by string? v)
                     (mapcat (fn [v]
                               (if (string? (first v))
                                 [(-> (str/join v)
                                      (str/replace #"\s+" " "))]
                                 v))))]
          (if (and (= 1 (count v)) (string? (first v)))
            (first v)
            (list* `str v)))
        :else (do
                (shared/warn-on-interpret v)
                `(~'yawn.compiler/maybe-interpret-class ~v))))

(comment
 (add-tap println)
 (= `(str ~'a " b " ~'c " " ~'d)
    (join-strings-compile '[a "b   " c d]))
 (= "a b c d" (join-strings-compile ["a" "b" "c" "d"])))

(defn compile-vec
  "Returns an unevaluated form that returns a react element"
  [v] (emit (analyze-vec v)))

(defn compile-or-interpret-child
  "Compiles hiccup forms & wraps ambiguous forms for runtime interpretation"
  [form]
  (if (map? form)
    (throw (ex-info "a map is an invalid child" {:form form}))
    (case (compile-mode form)
      :el form
      :interpret (do (shared/warn-on-interpret form)
                     `(~'yawn.convert/x ~form))
      :compile (compile-vec form)
      :maybe-interpret
      (or (wrap-return form compile-or-interpret-child)
          `(infer/maybe-interpret ~form)))))

(defn compile-hiccup-child
  "Only compiles 'obvious' potential hiccup forms, ie. vectors... other args
   are untouched."
  [form]
  (or (wrap-return form compile-hiccup-child)
      (case (compile-mode form)
        :compile (compile-vec form)
        :interpret `(~'yawn.convert/x ~form)
        form)))

(defn view-from-element
  ([el] (view-from-element el nil))
  ([original-el initial-props]
   (let [[el id class-string] (if (or (keyword? original-el)
                                      (string? original-el))
                                (convert/parse-tag (name original-el))
                                [original-el nil nil])
         initial-props (-> (convert/convert-props initial-props)
                           (cond-> id (assoc "id" id)
                                   class-string (update "className" #(if %
                                                                       (join-strings-compile [% class-string])
                                                                       class-string))))]
     `(let [initial-props# ~(literal->js initial-props)]
        (fn [new-props# & children#]
          (let [new-props?# (map? new-props#)
                props# (cond->> initial-props#
                                new-props?#
                                (~'yawn.convert/merge-js-props! (convert/convert-props new-props#)))
                children# (cond->> children#
                                   (not new-props?#)
                                   (cons new-props#))]
            (~'yawn.convert/make-element ~el props# children# 0)))))))

(defn emit
  "Emits the final react js code"
  [[tag props children {:as form-options
                        :keys [create-element?
                               id
                               class-string
                               key
                               ref
                               prop-mode
                               form-meta]}]]
  (let [runtime-static-props (fn [form]
                               ;; adds dynamic props to js-props object at runtime
                               (if-let [ops (-> (for [[k v] {"id" id "key" key "ref" ref}
                                                      :when v]
                                                  `(~'applied-science.js-interop/!set ~k ~v))
                                                (cond-> class-string
                                                        (conj `(~'yawn.convert/update-className ~class-string)))
                                                seq)]
                                 `(-> ~form ~@ops)
                                 form))]
    (if create-element?
      `(~'yawn.react/createElement
        ~tag
        ~(case prop-mode
           ;; literal, we can add static props at compile-time
           (:map :nil :no-props)
           (as-> (or props {}) props*
                 (dissoc props* :&)
                 (into props* (filter val) {:id id :key key :ref ref})
                 (convert/convert-props props*)
                 (cond-> props*
                         class-string (update "className" #(cond (nil? %) class-string
                                                                 (string? %) (str class-string " " %)
                                                                 :else `(~'clojure.core/str ~(str class-string " ") ~%))))
                 (literal->js props*)
                 (if (:& props)
                   `(-> ~props*
                        (~'applied-science.js-interop/extend!
                         (~'yawn.convert/convert-props ~(:& props))))
                   props*))
           ;; dynamic clj, need to interpret & then add static props
           :dynamic
           (runtime-static-props
            (when props
              `(~'yawn.convert/convert-props ~props)))
           ;; skip interpret, but add static props
           :js-object
           (runtime-static-props props))
        ~@(mapv compile-or-interpret-child children))
      ;; clj-element
      `(~'yawn.infer/maybe-interpret ~(with-meta `(~tag ~@(mapv compile-hiccup-child children)) form-meta)))))

(defn compile
  ([content] (compile nil content))
  ([env content]
   ;; TODO
   ;; handle ::js-ns/Item.x.y.z or ::clj-ns/Item.ns
   (binding [*env* env]
     (compile-or-interpret-child content))))

(defmacro x [content] (compile &env content))
(defmacro <> [content] (compile &env content))