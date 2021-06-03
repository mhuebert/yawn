(ns yawn.shared
  (:require #?(:cljs [applied-science.js-interop :as j])
            [clojure.string :as str]
            [yawn.util :as util]))

(defn join-strings [sep v]
  (if (vector? v)
    (str/join sep v)
    v))

(def camel-case
  (util/memo-by-string
   (fn [s]
     (cond-> s
             (not (or (str/starts-with? s "data-")
                      (str/starts-with? s "aria-")))
             (str/replace #"-(.)" (fn [[_ s]] (str/upper-case s)))))))

(defn camel-case-keys->map [m]
  (reduce-kv
   (fn [m k v]
     (assoc m (camel-case (name k)) v))
   {} m))

#?(:cljs
   (defn camel-case-keys->obj [m]
     (reduce-kv
      (fn [m k v]
        (j/!set m (camel-case (name k)) v))
      #js{} m)))

(defn warn-on-interpret [options expr]
  (when-not (:interpet (meta expr))
    (when (:warn-on-interpretation? options)
      (println (str "WARNING: interpreting form " (pr-str expr)
                    (let [{:keys [line file]} (meta expr)]
                      (when (and line file)
                        (str ", " file ":" line))))))
    (when (:throw-on-interpretation? options)
      (throw (ex-info "Interpreting form" {:form expr})))))

(defn join-strings-compile
  "Joins strings, space separated"
  [options sep v]
  (cond (string? v) v
        (vector? v)
        (if (every? string? v)
          (str/join sep v)
          `(~'clojure.core/str ~@(interpose sep v)))
        :else
        (do
          (warn-on-interpret options v)
          `(~'yawn.compiler/maybe-interpret-class ~(:js-options-sym options) ~v))))

(defn camel-case-keys-compile
  "returns map with keys camel-cased"
  [options m]
  (if (map? m)
    (camel-case-keys->map m)
    (do
      (warn-on-interpret options m)
      `(camel-case-keys->obj ~m))))

(defn format-style-prop->map [options v]
  (if (vector? v)
    (mapv (partial camel-case-keys-compile options) v)
    (camel-case-keys-compile options v)))