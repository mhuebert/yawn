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
  ;; props cases
  ;; :no-props           - form is handled as a child element
  ;; :map                - map is precompiled into props object
  ;; :compiled           - form is assumed to be a compiled props object
  ;; :to-convert         - form is converted to a props object at runtime
  ;; :maybe-props        - check if it is a child at runtime, otherwise convert to a props object
  (or (when (map? props) :map)
      (when (or (seq? props) (symbol? props))
        (let [props-meta (meta props)
              tag (or (:tag props-meta) (some->> *env* (infer/infer-type props)))
              tag (if (set? tag)
                    (let [tag (disj tag 'clj-nil)]
                      (if (= 1 (count tag))
                        (first tag)
                        tag))
                    tag)]
          (cond
            ;; [:div ^:props x]
            (:props props-meta) :to-convert
            ;; [:div ^props x]
            (= 'props tag) :to-convert
            ;; [:div (v/props ...)] tag is inferred from v/props fn
            (= 'compiled-props tag) :compiled
            ;; [:div ..child]
            (#{'cljs.core/IVector
               'yawn.view/el
               'string
               'number
               'clj-nil} tag) :none
            :else :maybe-props)))
      :none))

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
        :else `(~'yawn.compiler/maybe-interpret-class ~classv)))

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

(def props->js (comp literal->js convert-props))

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
            props? (not= mode :none)
            props-expr (case mode
                         :none (props->js static-props)
                         :map (do (assert (not (:& props)) "& deprecated, use v/props instead")
                                  (props->js (merge-props static-props props)))
                         :compiled (if static-props
                                     `(~'yawn.convert/merge-js-props! ~(props->js static-props) ~props)
                                     props)
                         :to-convert (if static-props
                                       `(~'yawn.convert/merge-js-props!
                                          ~(props->js static-props)
                                          (~'yawn.convert/convert-props ~props))
                                       `(~'yawn.convert/convert-props ~props))
                         :maybe-props (props->js static-props))]
        [tag
         props-expr
         (when (= mode :maybe-props) props)
         (children-as-list (cond-> body props? next))
         {:create-element? true
          :prop-mode       mode}])
      [tag nil nil body {:form-meta (meta vec)}])))


(comment
  (emit (analyze-vec [:div#foo 'a]))
  (analyze-vec [:div.a#foo])
  (analyze-vec [:h1.b {:className "a"}])
  (analyze-vec '[:div (for [x xs] [:span 1])]))

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

(defn from-element*
  ([kw]
   (let [[element tag-props] (parse-tag kw)
         element (or (shared/custom-elements element) element)]
     `(~'yawn.convert/partial-constructor ~element ~(props->js tag-props))))
  ([kw props-or-el]
   (let [[element tag-props] (parse-tag kw)
         element (or (shared/custom-elements element) element)]
     (if (= "createElement" element)
       `(~'yawn.convert/partial-constructor ~props-or-el ~(props->js tag-props))
       `(~'yawn.convert/partial-constructor ~element ~(props->js (merge-props tag-props props-or-el))))))
  ([kw el props]
   (let [[element tag-props] (parse-tag kw)
         element (or (shared/custom-elements element) element)]
     (assert (= "createElement" element))
     `(~'yawn.convert/partial-constructor ~el ~(props->js (merge-props tag-props props))))))

(defmacro dynamic-el [el]
  (if (= 'yawn.view/el (infer/infer-type el &env))
    el
    `(let [el# ~el]
       (if (or (string? el#) (keyword? el#))
         (~'yawn.convert/from-element el#)
         el#))))

(defn emit
  "Emits the final react js code"
  [[tag props maybe-props children {:as   form-options
                                    :keys [create-element?
                                           form-meta]}]]
  (if create-element?
    (if maybe-props
      `(let [maybe-props# ~maybe-props
             is-props?# (or (map? maybe-props#)
                            (and (~'cljs.core/object? maybe-props#)
                                 (not (~'cljs.core/unchecked-get maybe-props# "$$typeof")))) ;; to detect react elements and portals; maybe too broad
             props# ~props]
         (~'yawn.react/createElement
           ~tag
           (~'yawn.convert/merge-js-props! props# (when is-props?# (~'yawn.convert/convert-props maybe-props#)))
           (when-not is-props?#
             (~'yawn.convert/x maybe-props#))
           ~@(mapv compile-or-interpret-child children)))
      `(~'yawn.react/createElement
         ~tag
         ~props
         ~@(mapv compile-or-interpret-child children)))
    `(~'yawn.infer/maybe-interpret ~(with-meta `((dynamic-el ~tag)
                                                 ~@(mapv compile-hiccup-child children))
                                               form-meta))))
(defn compile
  ([content] (compile nil content))
  ([env content]
   ;; TODO
   ;; handle ::js-ns/Item.x.y.z or ::clj-ns/Item.ns
   (binding [*env* env]
     (compile-or-interpret-child content))))

(comment
  (compile '[:div#foo.c1 {:class x}])
  (convert-props (merge-props {:class "c1"}
                              '{:class x})))

(defmacro x [content] (compile &env content))
(defmacro <> [content] (compile &env content))