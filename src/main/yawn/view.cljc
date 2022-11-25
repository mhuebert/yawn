(ns yawn.view
  (:require #?@(:cljs [["react" :as react]
                       ["use-sync-external-store/shim" :refer [useSyncExternalStore]]])
            [clojure.string :as str]
            [clojure.walk :as walk]
            [applied-science.js-interop :as j]
            [yawn.env :as env]
            [yawn.compiler :as c]
            #?(:cljs [yawn.convert]))
  #?(:cljs (:require-macros yawn.view)))

(declare el) ;; for type hints

(def ^boolean refresh-enabled? #?(:cljs (exists? js/ReactRefreshRuntime)))

(env/def-options hiccup-opts {})

(defmacro x
  "Converts `form` to React element"
  [form]
  (c/compile hiccup-opts form))

(defn parse-args [args & preds]
  (loop [args args
         preds preds
         out []]
    (if (and (seq args) (seq preds))
      (let [p (first preds)]
        (if (p (first args))
          (recur (rest args)
                 (rest preds)
                 (conj out (first args)))
          (recur args
                 (rest preds)
                 (conj out nil))))
      (into out args))))

(defn- find-all [pred body]
  (let [sym-list (volatile! [])]
    (walk/postwalk
     (fn w [x]
       (if (pred x)
         (do (vswap! sym-list conj x)
             x)
         x))
     body)
    @sym-list))

(defn- maybe-hook?
  ;; detect forms that may be hooks based on syntax. this is
  ;; for enabling hot-reload to know when to clear state.
  [sym]
  (let [sym-name (name sym)]
    (or (identical? "deref" sym-name)
        (str/starts-with? sym-name "use"))))

(defn- hook-signature [body]
  ;; a string containing all symbols in body that begin with `use`
  ;; followed by - or any capital letter, joins to a string.
  (->> (find-all (every-pred symbol? maybe-hook?) body)
       (str/join "|")))

(defn simple-argv [argv]
  (mapv #(cond (symbol? %) %
               (map? %) (:as % (gensym "map"))
               (vector? %) (gensym "vec")
               :else (gensym)) argv))

(defn names [name]
  {:display (str *ns* \/ name)
   :sig (symbol (str name "-sig"))
   :constructor (symbol (str name "-constructor"))})


(defn refresh:create-sig []
  #?(:cljs (js/ReactRefreshRuntime.createSignatureFunctionForTransform)))

(defn refresh:after [sig constructor hook-sig fqn]
  #?(:cljs
     (do
       (sig constructor hook-sig nil nil)
       (.register js/ReactRefreshRuntime constructor fqn)
       (.performReactRefresh js/ReactRefreshRuntime))))

(defn defview:impl [name args]
  (let [[docstring opts argv & body] (parse-args args string? map?)
        key-fn (:key opts)
        name (vary-meta name assoc :tag 'yawn.view/el)
        simple-args (simple-argv argv)
        props-sym (gensym "props")
        names (names name)]
    `(let [refresh?# ~'yawn.view/refresh-enabled?
           ~(:sig names) (when refresh?# (refresh:create-sig))
           ~(:constructor names) (doto (j/fn ~(:constructor names) [~props-sym]
                                         (~@(if (seq argv)
                                              `(j/let [~argv (j/get ~props-sym :cljs-args)])
                                              `[do])
                                          (when refresh?# (~(:sig names)))
                                          ~@(drop-last body)
                                          (~'yawn.view/x ~(last body))))
                                   (j/assoc! :displayName ~(:display names)))]

       (defn ~name
         ~@(when docstring [docstring])
         {:arglists '(~(with-meta argv {:tag 'yawn.view/el}))}
         ~simple-args
         (~'yawn.react/createElement
          ~(:constructor names)
          (j/obj ~@(when key-fn [:key `(~key-fn ~@simple-args)])
                 :cljs-args [~@simple-args])))
       (when refresh?#
         (refresh:after ~(:sig names)
                        ~(:constructor names)
                        ~(hook-signature body)
                        ~(:display names)))
       #'~name)))


(defmacro defview [name & args]
  (defview:impl name args))