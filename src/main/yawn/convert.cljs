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
  (when p2
    (doseq [[k v] (js/Object.entries p2)]
      (case k "style" (j/!update p1 "style" #(if-not % v (j/merge! % v)))
              "className" (j/!update p1 "className" #(if-not % v (str % " " v)))
              (j/!set p1 k v))))
  p1)

(declare x)

(defprotocol IElement
  (to-element [form]))

(defn defined? [x]
  (not (undefined? x)))

(defn get-props [form i]
  (let [result (-nth form i js/undefined)]
    (if (undefined? result)
      result
      (if (or (and (object? result)
                   (not (react/valid-element? result)))
              (map? result))
        result
        js/undefined))))

(defn add-static-props [props-obj id class-string]
  (cond-> props-obj
          (defined? class-string) (update-className class-string)
          (defined? id) (applied-science.js-interop/assoc! :id id)))

(defn make-element
  "Returns a React element. `tag` may be a string or a React component (a class or a function).
   Children will be read from `form` beginning at index `start`."
  ([element-type form ^number prop-position]
   (let [props (-nth form prop-position js/undefined)
         props? (not (identical? js/undefined props))]
     (make-element element-type
                   (when props? (convert-props props))
                   form
                   (cond-> prop-position props? inc))))
  ([element-type props-obj form children-start]
   (let [form-count (count form)]
     (case (- form-count children-start) ;; fast cases for small numbers of children
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

(defn interpret-vec [form]
  (util/if-defined [form-0 (-nth form 0 js/undefined)]
    (if (keyword? form-0)
      (j/let [^js [tag id class-name] (shared/parse-tag (name form-0))
              tag (or (shared/custom-elements tag) tag)
              create-element? (identical? tag "createElement")
              element (if create-element? (-nth form 1) tag)
              prop-position (if create-element? 2 1)
              props (get-props form prop-position)
              props? (defined? props)
              props (-> props
                        (cond-> props? convert-props)
                        (add-static-props id class-name))
              children-start (cond-> prop-position
                                     props? inc)]
        (make-element element
                      props
                      form
                      children-start))
      (x (apply form-0 (rest form))))
    form))

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
