(ns yawn.shared
  (:require #?(:cljs [applied-science.js-interop :as j])
            #?(:clj [net.cgrand.macrovich :as m])
            [clojure.string :as str]
            [yawn.util :as util])
  #?(:cljs (:require-macros [net.cgrand.macrovich :as m]
                            [yawn.shared :refer [clj']])))

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
       #?(:cljs #js[tag-name nil nil] :clj [tag-name nil nil])
       (let [pattern #"([^#.]+)?(?:#([^.]+))?(?:\.(.*))?"]
         #?(:cljs (-> (.exec pattern tag-name)
                      (.slice 1 4)
                      (j/update! 2 #(if % (dots->spaces %) %)))
            :clj  (-> (rest (re-find pattern tag-name))
                      vec
                      (update 2 #(when % (dots->spaces %))))))))))

(defmacro clj' [x] (m/case :clj `'~x :cljs x))

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