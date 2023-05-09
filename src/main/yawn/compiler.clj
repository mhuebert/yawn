(ns yawn.compiler
  "
  Hicada - Hiccup compiler aus dem Allgaeu

  NOTE: The code for has been forked like this:
  weavejester/hiccup -> r0man/sablono -> Hicada."
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [yawn.wrap-return :refer [wrap-return]]
   [yawn.infer :as infer]
   [yawn.util :as util]
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
              tag (or (:tag props-meta) (some->> *env* (infer/infer-type props)))]
          (cond (or (:props props-meta) (= 'props tag)) :dynamic
                (#{'object 'js} tag) :js-props)))
      :no-props))

(defn literal->js
  "Efficiently emit to literal JS form"
  [x]
  (cond
    (nil? x) x
    (keyword? x) (name x)
    (string? x) x
    (vector? x) (apply list 'cljs.core/array (mapv literal->js x))
    (map? x) (when (seq x)
               `(~'js-obj ~@(->> x (apply concat) (map literal->js))))
    :else x))

(defn remove-empty [m]
  (not-empty (into {} (filter (comp (fn [x] (if (coll? x)
                                              (seq x)
                                              (some? x))) val)) m)))

(defn parse-tag [tag]
  (if (or (keyword? tag) (string? tag))
    (let [[el id class-string] (shared/parse-tag (name tag))]
      [el (remove-empty {:id id :class class-string})])
    [tag nil]))

(defn analyze-vec
  "Given:
  [:div.x.y#id (other)]
  Returns:
  [:div {:id \"id\"
         :class [\"x\" \"y\"]}
    (other)]"
  [[tag & body :as vec]]
  (let [[tag tag-props] (parse-tag tag)
        static-props (remove-empty (merge tag-props (select-keys (meta vec) [:ref :key])))
        tag-override (shared/custom-elements tag)
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
           :static-props static-props
           :prop-mode mode})])
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

(defn merge-classes
  "Returns vector of classes from list of strings, expressions or vectors (containing strings/expressions)"
  ([c1 c2 & more] (reduce merge-classes (merge-classes c1 c2) more))
  ([c1 c2]
   (let [c1 (if (vector? c1) c1 [c1])]
     (into []
           (keep identity)
           (if (vector? c2)
             (into c1 c2)
             (conj c1 c2))))))

(comment
 (merge-classes nil "b")
 (merge-classes "a" nil)
 (merge-classes "a" "b")
 (merge-classes ["a" "b"] "c")
 (merge-classes ["a"] ["b"])
 (merge-classes ["b"] 'c))

(defn compile-classv [classv]
  (cond (string? classv) (str/replace classv #"\s+" " ")
        (vector? classv)
        (let [out (->> classv
                       (remove #(and (string? %) (str/blank? %)))
                       (interpose " ")
                       (partition-by string?)
                       (mapcat (fn [v]
                                 (if (string? (first v))
                                   [(-> (str/join v)
                                        (str/replace #"\s+" " "))]
                                   (map (fn [v] `(~'yawn.compiler/maybe-interpret-class ~v)) v)))))]
          (if (= 1 (count out))
            (first out)
            `(str ~@out)))
        :else  `(~'yawn.compiler/maybe-interpret-class ~classv)))

(defn camel-case-keys-compile
  "returns map with keys camel-cased"
  [m]
  (if (map? m)
    (shared/camel-case-keys m)
    `(shared/camel-case-keys ~m)))

(defn format-style-prop->map [v]
  (if (vector? v)
    (mapv camel-case-keys-compile v)
    (camel-case-keys-compile v)))

(defn convert-props [props]
  (reduce-kv
   (fn [m k v]
     (if (symbol? k)
       (assoc m `(shared/camel-case (name ~k)) v)
       (let [kname (name k)]
         (case kname
           "class"
           (assoc m "className" (compile-classv v))
           "for"
           (assoc m "htmlFor" v)
           "style"
           (assoc m "style" (format-style-prop->map v))
           (let [k (if (string? k)
                     k
                     (shared/camel-case kname))]
             (assoc m k v))))))
   {}
   props))

(comment
 (compile '[:div.c1 {:class x}])
 (compile-classv '(when foo "x"))
 (compile-classv '["a" b " "])
 (compile-classv '[c "a"])
 (compile-classv '["a" "b" "c"]))

(defn merge-styles [m1 m2]
  (if (and (map? m1) (map? m2))
    (merge m1 m2)
    `(merge ~m1 ~m2)))

(defn merge-props
  ([p1 p2 & more]
   (reduce merge-props (merge-props p1 p2) more))
  ([p1 p2]
   (if p2
     (reduce-kv (fn [out k v]
                  (case k :style (update out k merge-styles v)
                          :class (update out k merge-classes v)
                          (assoc out k v))) p1 p2)
     p1)))

(comment
 (merge-props {:id 1 :class ["a" "b"]}
              {:id 2 :class 'c}
              {:id 3 :class "d"})

 (merge-props {:id 1} nil)

 (merge-props nil {:id 1}))

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
   (let [[el tag-props] (parse-tag original-el)
         initial-props (convert-props (merge-props initial-props tag-props))]
     `(let [initial-props# ~(literal->js initial-props)]
        (fn [new-props# & children#]
          (let [new-props?# (map? new-props#)
                props# (cond->> initial-props#
                                new-props?#
                                (~'yawn.convert/merge-js-props! (convert-props new-props#)))
                children# (cond->> children#
                                   (not new-props?#)
                                   (cons new-props#))]
            (~'yawn.convert/make-element ~el props# children# 0)))))))

(def props->js (comp literal->js convert-props))

(defn emit
  "Emits the final react js code"
  [[tag props children {:as form-options
                        :keys [create-element?
                               static-props
                               prop-mode
                               form-meta]}]]
  (if create-element?
    `(~'yawn.react/createElement
      ~tag
      ~(case prop-mode
         ;; no props, only use tag-props
         (:nil :no-props)
         (props->js static-props)

         ;; literal props, merge and compile
         :map
         (do (assert (not (:& props)) "& deprecated, use v/props instead")
             (props->js (merge-props static-props props)))

         ;; runtime props, merge with compiled static-props at runtime
         :dynamic
         (cond->> `(~'yawn.convert/convert-props ~props)
                  static-props
                  (list 'yawn.convert/merge-js-props! (props->js static-props)))
         ;; skip interpret, but add static props
         :js-props
         (cond->> props
                  static-props
                  (list 'yawn.convert/merge-js-props! (props->js static-props))))
      ~@(mapv compile-or-interpret-child children))
    ;; clj-element
    `(~'yawn.infer/maybe-interpret ~(with-meta `(~tag ~@(mapv compile-hiccup-child children)) form-meta))))

(comment
 (compile '[:div.c1 {:class x}])
 (convert-props (merge-props {:class "c1"}
                             '{:class x})))
(defn compile
  ([content] (compile nil content))
  ([env content]
   ;; TODO
   ;; handle ::js-ns/Item.x.y.z or ::clj-ns/Item.ns
   (binding [*env* env]
     (compile-or-interpret-child content))))

(defmacro x [content] (compile &env content))
(defmacro <> [content] (compile &env content))