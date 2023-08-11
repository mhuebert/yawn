(ns yawn.convert
  (:require [applied-science.js-interop :as j]
            [clojure.string :as str]
            [yawn.util :as util]
            [yawn.react :as react]
            [yawn.shared :as shared]))

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

(defn ^string class-string [v]
  (cond->> v (vector? v) (str/join " ")))

(defn- update-class* [x class-str]
  (if (some? x)
    (str class-str " " x)
    class-str))

(defn update-className [obj class-str]
  (j/update! obj :className update-class* class-str))

(defn join-strings [sep v]
  (if (vector? v)
    (str/join sep v)
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse props (top-level)

(defn convert-props [props]
  (if
   (object? props)
    props
    (reduce-kv
     (fn [m k v]
       (let [kname (name k)]
         (case kname
           "class"
           (j/!set m "className" (cond->> v (vector? v) (str/join " ")))
           "for"
           (j/!set m "htmlFor" v)
           "style"
           (j/!set m "style" (shared/camel-case-keys v))
           (let [^string k (if (string? k)
                             k
                             (shared/camel-case kname))]
             (j/!set m k v)))))
     #js{}
     props)))

(defn merge-clj-classes
  [c1 c2]
  (str (cond->> c1 (vector? c1) (str/join " "))
       " "
       (cond->> c2 (vector? c2) (str/join " "))))

(defn merge-js-props!
  "Copies properties from p2 to p1 (overwriting), merges `className` and `style`"
  [p1 p2]
  (cond (not p1) p2
        (not p2) p1
        :else
        (do (doseq [[k v] (js/Object.entries p2)]
              (case k "style" (j/!update p1 "style" #(if-not % v (j/merge! % v)))
                      "className" (j/!update p1 "className" #(if-not % v (str % " " v)))
                      (j/!set p1 k v)))
            p1)))

(declare x)

(defprotocol IElement
  (to-element [form]))

(defn defined? [x]
  (not (undefined? x)))

(defn get-props [form i]
  (if (nil? form)
    js/undefined
    (let [result (nth form i js/undefined)]
      (if (undefined? result)
        result
        (if (or (and (object? result)
                     (not (react/valid-element? result)))
                (map? result))
          result
          js/undefined)))))

(defn add-static-props [props-obj id class-string]
  (cond-> props-obj
          (defined? class-string) (update-className class-string)
          (defined? id) (applied-science.js-interop/assoc! :id id)))

(defn make-element
  "Returns a React element. `tag` may be a string or a React component (a class or a function).
   Children will be read from `form` beginning at index `start`."
  ([element-type form ^number prop-position]
   (let [props (nth form prop-position js/undefined)
         props? (not (identical? js/undefined props))]
     (make-element element-type
                   (when props? (convert-props props))
                   form
                   (cond-> prop-position props? inc))))
  ([element-type props-obj form children-start]
   (let [form-count (count form)]
     (case (- form-count children-start)                    ;; fast cases for small numbers of children
       0 (.call react/createElement nil element-type props-obj)
       1 (.call react/createElement nil element-type props-obj (x (nth form children-start)))
       2 (.call react/createElement nil element-type props-obj (x (nth form children-start)) (x (nth form (+ children-start 1))))
       3 (.call react/createElement nil element-type props-obj (x (nth form children-start)) (x (nth form (+ children-start 1))) (x (nth form (+ children-start 2))))
       (let [out #js[element-type props-obj]]
         (loop [i children-start]
           (if (== i form-count)
             (.apply react/createElement nil out)
             (do
               (.push out (x (nth form i)))
               (recur (inc i))))))))))

(defn partial-constructor [element initial-js-props]
  (fn [& args]
    (let [element? (identical? "createElement" element)
          props-i (if element? 1 0)
          new-props (get-props args props-i)
          new-props? (defined? new-props)
          children-i (if new-props? (inc props-i) props-i)
          props (cond-> (j/extend! #js{} initial-js-props)
                        new-props?
                        (merge-js-props! (convert-props new-props)))]
      (make-element (if element? (first args) element)
                    props
                    args
                    children-i))))

(defn static-props-obj
  [id class-name]
  (cond-> #js{}
          id (j/!set :id id)
          class-name (j/!set :className class-name)))

(defn parse-element [kw-or-el]
  (if (keyword? kw-or-el)
    (shared/parse-tag (name kw-or-el))
    #js[kw-or-el]))

(defn from-element
  ([kw]
   (j/let [^js [tag id class-name] (parse-element kw)]
     (partial-constructor (or (shared/custom-elements tag) tag)
                          (static-props-obj id class-name))))
  ([kw props-or-el]
   (j/let [^js [tag id class-name] (parse-element kw)
           element (or (shared/custom-elements tag) tag)]
     (if (= "createElement" element)
       (partial-constructor props-or-el (static-props-obj id class-name))
       (partial-constructor element (merge-js-props! (static-props-obj id class-name) (convert-props props-or-el))))))
  ([kw element props]
   (j/let [^js [_ id class-name] (parse-element kw)]
     (partial-constructor element (merge-js-props! (static-props-obj id class-name) (convert-props props))))))

(comment
 (from-element :el 'js/What {:class "foo"})
 ((from-element :div#foo.x.y {}))
 (parse-element :div#foo.x.y)
 (from-element :el js/What {})

 )

(defn interpret-vec [form]
  (try
    (util/if-defined [form-0 (nth form 0 js/undefined)]
      (if (keyword? form-0)
        (apply (from-element form-0) (rest form))
        (x (apply form-0 (rest form))))
      form)
    (catch js/Error e
      (prn :error/interpret-vec form)
      (throw e))))

(defn x
  "Convert form to a React element"
  [form]
  (cond (react/valid-element? form) form
        (vector? form) (interpret-vec form)
        (seq? form) (make-element react/Fragment nil (vec form) 0)
        (array? form) (.apply react/createElement nil form)
        (satisfies? IElement form) (to-element form)
        :else form))
(defn <> [form] (x form))

(comment
 (x [:div])
 (x [:div.a])
 (x [:div.a {:class "b"}])
 (x [:div.a {:class ["b" "c"]}])
 (x [:div "a"])
 (x [:div #js{} "a"])

 (defn my-fn [x] [:div x])
 (x [my-fn "a"])
 (x [my-fn [:div.x]])

 (x [:div nil "a"])

 (x [:... "a" "b"])
 (x [:... [:div]])

 )
