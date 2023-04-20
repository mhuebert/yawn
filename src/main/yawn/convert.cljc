(ns yawn.convert
  (:require #?(:clj [net.cgrand.macrovich :as m])
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [yawn.util :as util]
            [yawn.react :as react]
            [yawn.shared :as shared])
  #?(:cljs (:require-macros [yawn.convert :refer [clj']]
                            [net.cgrand.macrovich :as m])))

(defmacro clj' [x] (m/case :clj `'~x :cljs x))

(def rewrite-for? true)

(defn custom-elements [k]
  (case k
    "<>" (clj' yawn.react/Fragment)
    "..." (clj' yawn.react/Fragment)
    ">" "createElement"
    "el" "createElement"
    "Fragment" (clj' yawn.react/Fragment)
    "Suspense" (clj' yawn.react/Suspense)
    "Portal" (clj' yawn.react/Portal)
    nil))

(defn add-prop [m k v]
  (let [kname (name k)]
    (case kname
      "class"
      #?(:clj  (assoc m "className" (yawn.shared/join-strings-compile " " v))
         :cljs (j/!set m "className" (yawn.shared/join-strings " " v)))
      "for"
      #?(:clj  (assoc m "htmlFor" v)
         :cljs (j/!set m "htmlFor" v))
      "style"
      #?(:clj  (assoc m "style" (yawn.shared/format-style-prop->map v))
         :cljs (j/!set m "style" (yawn.shared/camel-case-keys v)))
      "&" (reduce-kv yawn.convert/add-prop m v)
      (let [k (if (string? k)
                k
                (shared/camel-case kname))]
        #?(:cljs (doto m (unchecked-set k v))
           :clj  (assoc m k v))))))

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
         :clj  class-string) [v]
  (cond (string? v) v
        (vector? v) (str/join " " v)
        :else v))

(defn- update-class* [x class-str]
  (if (some? x)
    (str class-str " " x)
    class-str))

#?(:cljs
   (defn update-className [obj class-str]
     (j/update! obj :className update-class* class-str)))

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
;; parse props (top-level)

(defn convert-props [props]
  #?(:cljs (if
            (object? props)
             props
             (reduce-kv add-prop #js{} props))
     :clj  (reduce-kv add-prop {} props)))

#?(:cljs
   (defn merge-js-props!
     "Copies properties from p2 to p1 (overwriting), merges `className` and `style`"
     [p1 p2]
     (doseq [[k v] (js/Object.entries p2)]
       (case k "style" (j/!update p1 "style" #(if-not % v (j/merge! % v)))
               "className" (j/!update p1 "className" #(if-not % v (str % " " v)))
               (j/!set p1 k v)))
     p1))

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
          (j/let [^js [tag id class-name] (parse-tag (name form-0))
                  tag (or (custom-elements tag) tag)
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
    (defn <> [form] (x form))))

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
