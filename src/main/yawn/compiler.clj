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
        (let [props-meta (meta props)]
          (cond (:props props-meta) :dynamic
                (#{'object
                   'js} (:tag props-meta)) :js-object)))
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

(defn join-strings-compile
  "Joins strings, space separated"
  [sep v]
  (cond (string? v) v
        (vector? v)
        (if (every? string? v)
          (str/join sep v)
          `(~'clojure.core/str ~@(interpose sep v)))
        :else
        (do
          (shared/warn-on-interpret v)
          `(~'yawn.compiler/maybe-interpret-class ~v))))

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

(defn compile-props
  [props]
  (reduce-kv
   (fn [m k v]
     (convert/add-prop m k v))
   {}
   props))


(defn compile-element
  ([el] (compile-element el nil))
  ([el initial-props]
   (let [[el id class-string] (if (or (keyword? el)
                                      (string? el))
                                (convert/parse-tag (name el))
                                [el nil nil])
         initial-props (some-> initial-props compile-props)
         initial-className (some->> [class-string (get initial-props "className")]
                               (keep identity)
                               seq
                               (str/join " "))
         initial-style (some-> (get initial-props "style") literal->js)
         initial-props (-> initial-props (dissoc "className" "style") literal->js)]
     (tap> [:ip initial-props :s initial-style :c initial-className])
     `(let [update-props# (fn [props#]
                            (-> props#
                                ~@(when id [`(j/!set :id ~id)])
                                ~@(when initial-className [`(~'yawn.convert/update-className ~initial-className)])
                                ~@(when initial-props [`(j/extend! ~initial-props)])
                                ~@(when initial-style [`(j/update! :style j/extend! ~initial-style)])))]
        (fn [props# & children#]
          (let [[props# children#] (if (map? props#)
                                     [(convert/interpret-props props#) children#]
                                     [(cljs.core/js-obj) (cons props# children#)])]
            (~'yawn.convert/make-element ~el (update-props# props#) children# 0)))))))

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
                 (compile-props props*)
                 (cond-> props*
                         class-string (update "className" #(cond (nil? %) class-string
                                                                 (string? %) (str class-string " " %)
                                                                 :else `(~'clojure.core/str ~(str class-string " ") ~%))))
                 (literal->js props*)
                 (if (:& props)
                   `(-> ~props*
                        (~'applied-science.js-interop/extend!
                         (~'yawn.convert/interpret-props ~(:& props))))
                   props*))
           ;; dynamic clj, need to interpret & then add static props
           :dynamic
           (runtime-static-props
            (when props
              `(~'yawn.convert/interpret-props ~props)))
           ;; skip interpret, but add static props
           :js-object
           (runtime-static-props props))
        ~@(mapv compile-or-interpret-child children))
      ;; clj-element
      `(~'yawn.infer/maybe-interpret ~(with-meta `(~tag ~@(mapv compile-hiccup-child children)) form-meta)))))

(defn compile
  [content]
  ;; TODO
  ;; handle ::js-ns/Item.x.y.z or ::clj-ns/Item.ns
  (when-let [env cljs.env/*compiler*]
    (let [{:keys [js-deps js-aliases]} (ana/get-namespace env (ns-name *ns*))
          js-aliases (->> js-deps
                          (map (juxt (comp str :as val) (comp js-aliases key)))
                          (into {}))]
      ;; desired:
      ;; resolve ::menu/Item.x.y.z to the js module, apply classes.
      '{"menu" module$node_modules$$radix_ui$react_menubar$dist$index,
        "ava" module$node_modules$$radix_ui$react_avatar$dist$index}))

  (compile-or-interpret-child content))

(defmacro x [content] (compile content))
(defmacro <> [content] (compile content))