(ns yawn.convert
  (:require #?@(:cljs [[applied-science.js-interop :as j]
                       ["react" :as React]]
                :clj  [[net.cgrand.macrovich :as m]])
            [clojure.string :as str]
            [yawn.env :as env]
            [yawn.util :as util]
            [yawn.react :as react]
            [yawn.shared :as shared])
  #?(:cljs (:require-macros yawn.convert
                            [net.cgrand.macrovich :as m])))

(defn warn-on-interpret [options expr]
  (when-not (:interpet (meta expr))
    (when (:warn-on-interpretation? options)
      (println (str "WARNING: interpreting form " (pr-str expr)
                    (let [{:keys [line file]} (meta expr)]
                      (when (and line file)
                        (str ", " file ":" line))))))
    (when (:throw-on-interpretation? options)
      (throw (ex-info "Interpreting form" {:form expr})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key transformation

(def camel-case
  (util/memo-by-string
   (fn [s]
     (cond-> s
             (not (or (str/starts-with? s "data-")
                      (str/starts-with? s "aria-")))
             (str/replace #"-(.)" (fn [[_ s]] (str/upper-case s)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class string transformation

(defn #?(:cljs ^string class-string
         :clj  class-string) [classes]
  (cond (string? classes) classes
        (vector? classes) (str/join " " classes)
        :else classes))

(defn- update-class* [x class-str]
  (if (some? x)
    (str class-str " " x)
    class-str))

#?(:cljs
   (defn update-class->obj [obj class-str]
     (j/update! obj :className update-class* class-str)))

(defn update-class->map [obj class-str]
  (update obj :className update-class* class-str))

(defn join-strings [sep v]
  (if (vector? v)
    (str/join sep v)
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse static tag names

(def ^:private dot-pattern #?(:cljs (js/RegExp "\\." "g")
                              :clj  "."))

(defn replace-pattern [s pattern rep]
  #?(:clj  (str/replace s pattern rep)
     :cljs (.replace s (j/!set pattern :lastIndex 0) rep)))

(defn dots->spaces [s]
  (replace-pattern s dot-pattern " "))

(def parse-tag
  "Returns array of [tag-name, id, classes] from a tag-name like div#id.class1.class2"
  (util/memo-by-string
   (fn [tag-name]
     (if (= tag-name "...")
       [tag-name nil nil]
       (let [pattern #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?"]
         #?(:cljs (-> (.exec pattern tag-name)
                      (.slice 1 4)
                      (j/update! 2 #(if % (dots->spaces %) %)))
            :clj  (-> (rest (re-find pattern tag-name))
                      vec
                      (update 2 #(when % (dots->spaces %))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse style props

#?(:cljs
   (defn format-style-prop->object [v]
     (if (vector? v)
       (mapv shared/camel-case-keys->obj v)
       (shared/camel-case-keys->obj v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse props (top-level)

#?(:cljs
   (defn add-prop->object [options prop-handlers m k v]
     {:pre [(object? prop-handlers)]}
     (let [kname (if (string? k)
                   k (shared/camel-case (name k)))]
       (if-some [handler (j/!get prop-handlers kname)]
         (handler options prop-handlers m kname v)
         (j/!set m kname v)))))

(defn add-prop->map [options prop-handlers m k v]
  {:pre [(map? prop-handlers)]}
  (let [kname (if (string? k)
                k (shared/camel-case (name k)))]
    (if-some [handler (get prop-handlers kname)]
      (handler options prop-handlers m kname v)
      (assoc m kname v))))

(defn interpret-props [options props]
  #?(:cljs (if
            (object? props)
             props
             (let [handlers (j/!get options :prop-handlers)]
               (reduce-kv
                (fn [m k v] (add-prop->object options handlers m k v))
                #js{}
                props)))
     :clj  (let [handlers (get options :prop-handlers)]
             (reduce-kv
              (fn [m k v] (add-prop->map options handlers m k v))
              {}
              props))))


(env/set-defaults!
 '{;; settings for the compiler:
   :warn-on-interpretation? true
   :skip-types #{number
                 string
                 function
                 js
                 yawn.react/element}
   :rewrite-for? true

   ;; relevant for the interpreter:
   :custom-elements {"Fragment" yawn.react/Fragment
                     "<>" yawn.react/Fragment
                     "..." yawn.react/Fragment
                     "Suspense" yawn.react/Suspense
                     ">" "yawn/create-element"}
   :prop-handlers {"class"
                   (fn [options handlers m k v]
                     {:compile (assoc m "className" (yawn.shared/join-strings-compile options " " v))
                      :interpret (applied-science.js-interop/!set m "className" (yawn.shared/join-strings " " v))})
                   "for"
                   (fn [options handlers m k v]
                     {:compile (assoc m "htmlFor" v)
                      :interpret (applied-science.js-interop/!set m "htmlFor" v)})
                   "style"
                   (fn [options handlers m k v]
                     {:compile (assoc m k (yawn.shared/format-style-prop->map options v))
                      :interpret (applied-science.js-interop/!set m k (yawn.convert/format-style-prop->object v))})
                   "&"
                   (fn [options handlers m k v]
                     (reduce-kv (fn [m k v] ({:compile yawn.convert/add-prop->map
                                              :interpret yawn.convert/add-prop->object}
                                             options handlers m k v)) m v))}})

(env/def-options defaults {})

(declare x)

(defprotocol IElement
  (to-element [form]))

#?(:cljs
   (m/usetime

    (defn defined? [x]
      (not (undefined? x)))

    (defn get-props [form i]
      (let [result (-nth form i js/undefined)]
        (if (undefined? result)
          result
          (if (or (and (object? result)
                       (not (React/isValidElement result)))
                  (map? result))
            result
            js/undefined))))

    (j/defn add-static-props [props-obj ^:js [tag-name id class-string]]
      (cond-> props-obj
              (defined? class-string) (update-class->obj class-string)
              (defined? id) (applied-science.js-interop/!set :id id)))

    (defn make-element
      "Returns a React element. `tag` may be a string or a React component (a class or a function).
       Children will be read from `form` beginning at index `start`."
      ([options element-type form ^number prop-position]
       (let [props (-nth form prop-position js/undefined)
             props? (not (identical? js/undefined props))]
         (make-element options
                       element-type
                       (when props?
                         (interpret-props options props))
                       form
                       (cond-> prop-position props? inc))))
      ([options element-type props-obj form children-start]
       (let [form-count (count form)]
         (case (- form-count children-start) ;; fast cases for small numbers of children
           0 (.call react/createElement nil element-type props-obj)
           1 (.call react/createElement nil element-type props-obj (x options (nth form children-start)))
           2 (.call react/createElement nil element-type props-obj (x options (nth form children-start)) (x options (nth form (+ children-start 1))))
           3 (.call react/createElement nil element-type props-obj (x options (nth form children-start)) (x options (nth form (+ children-start 1))) (x options (nth form (+ children-start 2))))
           (let [out #js[element-type props-obj]]
             (loop [i children-start]
               (if (== i form-count)
                 (.apply react/createElement nil out)
                 (do
                   (.push out (x options (nth form i)))
                   (recur (inc i))))))))))

    (defn interpret-vec [options form]
      (util/if-defined [form-0 (-nth form 0 js/undefined)]
        (if (keyword? form-0)
          (let [tag (j/get-in options [:custom-elements (name form-0)]
                              (if (keyword? form-0)
                                (name form-0)
                                form-0))]
            (j/let [create-element? (identical? tag "yawn/create-element")
                    tag (if create-element? (-nth form 1) tag)
                    prop-position (if create-element? 2 1)
                    props (get-props form prop-position)
                    props? (defined? props)
                    static-tag? (string? tag)
                    parsed-tag (when static-tag?
                                 (parse-tag tag))
                    tag (if static-tag?
                          (aget parsed-tag 0)
                          tag)
                    props (cond-> props
                                  props?
                                  (->> (interpret-props options))
                                  (string? tag)
                                  (add-static-props parsed-tag))
                    children-start (cond-> prop-position
                                           props? inc)]
              (make-element options
                            tag
                            props
                            form
                            children-start)))
          (x options (apply form-0 (rest form))))
        form))

    (defn x
      ([form]
       (x defaults form))
      ([options form]
       (cond (vector? form) (interpret-vec options form)
             (seq? form) (make-element options react/Fragment nil (vec form) 0)
             (array? form) (.apply react/createElement nil form)
             (satisfies? IElement form) (to-element form)
             :else form)))))

(comment
 (x [:div])
 (x [:div.a])
 (x [:div.a {:class "b"}])
 (x [:div.a {:class ["b" "c"]}])
 (x [:div "a"])
 #?(:cljs (x [:div #js{} "a"]))

 (defn my-fn [x] [:div x])
 (x [my-fn "a"])
 (x [my-fn [:div.x]])

 (x [:div nil "a"])

 (x [:... "a" "b"])
 (x [:... [:div]])

 )
