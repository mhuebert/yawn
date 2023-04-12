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

(defn camel-case-keys [m]
  #?(:clj
     (reduce-kv
      (fn [m k v]
        (assoc m (if (string? k) k (camel-case (name k))) v))
      {} m)
     :cljs
     (reduce-kv
      (fn [m k v]
        (j/!set m (if (string? k) k (camel-case (name k))) v))
      #js{} m)))

(def ^:dynamic *throw-on-interpret* false)
(def ^:dynamic *warn-on-interpret* false)

(defn warn-on-interpret [expr]
  (when-not (:interpet (meta expr))
    (when *throw-on-interpret*
      (throw (ex-info "Inferred form" {:form expr})))
    (when *warn-on-interpret*
      (println (str "WARNING: interpreting form " (pr-str expr)
                    (let [{:keys [line file]} (meta expr)]
                      (when (and line file)
                        (str ", " file ":" line))))))))

(defn join-strings-compile
  "Joins strings, space separated"
  [sep v]
  (cond (string? v) v
        (vector? v)
        (if (every? string? v)
          (str/join sep v)
          `(~'clojure.core/str ~@(interpose sep v)))
        :else
        `(~'yawn.compiler/maybe-interpret-class ~v)))

(defn camel-case-keys-compile
  "returns map with keys camel-cased"
  [m]
  (if (map? m)
    (camel-case-keys m)
    (do
      (warn-on-interpret m)
      `(camel-case-keys ~m))))

(defn format-style-prop->map [v]
  (if (vector? v)
    (mapv camel-case-keys-compile v)
    (camel-case-keys-compile v)))