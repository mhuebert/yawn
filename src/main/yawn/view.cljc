(ns yawn.view
  (:require #?(:cljs ["react" :as react])
            [clojure.string :as str]
            [clojure.walk :as walk]
            [cljs.analyzer :as ana]
            [applied-science.js-interop :as j]
            [yawn.env :as env]
            #?@(:clj [[yawn.compiler :as c]
                      [net.cgrand.macrovich :as m]])
            #?(:cljs [yawn.convert]))
  #?(:cljs (:require-macros [net.cgrand.macrovich :as m]
                            [yawn.compiler :as c]
                            yawn.view)))

(declare el) ;; for type hints

(def ^boolean refresh-enabled? #?(:cljs (exists? js/ReactRefreshRuntime)))

(env/def-options hiccup-opts {})

(m/deftime

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
   (let [sym-list (atom [])]
     (walk/postwalk
      (fn w [x]
        (if (pred x)
          (do (swap! sym-list conj x)
              x)
          x))
      body)
     @sym-list))

 (defn- maybe-hook?
   ;; detect forms that may be hooks based on syntax. this is
   ;; for enabling hot-reload to know when to clear state.
   [sym]
   (let [sym-name (name sym)]
     (or
      (identical? "deref" sym-name)
      (str/starts-with? sym-name "use")
      (some->> (ana/resolve-symbol sym)
               (namespace)
               (str/includes? "hook")))))

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

 (defmacro x [form]
   (c/compile hiccup-opts form))

 (defn qname [name] (str *ns* \/ name))

 (defn refresh:sig [name] (symbol (str name "-sig")))
 (defn sym:ctor [name] (symbol (str name "-ctor")))

 (defn refresh:bindings [name]
   `[~(refresh:sig name) (when ~'yawn.view/refresh-enabled?
                           (js/ReactRefreshRuntime.createSignatureFunctionForTransform))])

 (defn refresh:inner [name]
   `(when ~'yawn.view/refresh-enabled? (~(refresh:sig name))))

 (defn refresh:after [name body]
   `(when ~'yawn.view/refresh-enabled?
      (~(refresh:sig name) ~(sym:ctor name) ~(hook-signature body) nil nil)
      (.register js/ReactRefreshRuntime ~(sym:ctor name) ~(qname name))
      (.performReactRefresh js/ReactRefreshRuntime)))

 (defmacro defview [name & args]
   (let [[docstring opts argv & body] (parse-args args string? map?)
         key-fn (:key opts)
         name (vary-meta name assoc :tag `el)
         simple-args (simple-argv argv)
         props-sym (gensym "props")]
     `(let [~@(refresh:bindings name)
            ~(sym:ctor name) (doto (j/fn ~(sym:ctor name) [~props-sym]
                                     (~@(if (seq argv)
                                          `(j/let [~(cond-> argv
                                                            (= 1 (count argv))
                                                            first)
                                                   (j/!get ~props-sym :children)])
                                          `[do])
                                      ~(refresh:inner name)
                                      ~@(drop-last body)
                                      (c/x hiccup-opts ~(last body))))
                               (j/!set :displayName ~(qname name)))]

        (defn ~name
          ~@(when docstring [docstring])
          {:arglists (~(with-meta argv {:tag 'yawn.view/el}))}
          ~simple-args
          (~'yawn.react/createElement
           ~(sym:ctor name)
           ~(when key-fn `(j/obj :key (~key-fn ~(first simple-args))))
           ~@simple-args))

        ~(refresh:after name body)

        #'~name))))


;; React API access
#?(:cljs
   (do
     (defn- as-array [x] (cond-> x (not (array? x)) to-array))

     (defn use-memo
       ([f] (react/useMemo f #js[]))
       ([f deps] (react/useMemo f (as-array deps))))

     (defn use-callback [x] (react/useCallback x))

     (defn- wrap-effect [f] #(or (f) js/undefined))

     (defn use-effect
       ([f] (react/useEffect (wrap-effect f) #js[]))
       ([f deps] (react/useEffect (wrap-effect f) (as-array deps))))

     (defn use-state [init] (react/useState init))

     (defn specify-atom! [obj]
       (specify! obj
         IDeref
         (-deref [^js this] (.-current this))
         IReset
         (-reset! [^js this new-value] (set! (.-current this) new-value))
         ISwap
         (-swap!
           ([o f] (reset! o (f o)))
           ([o f a] (reset! o (f o a)))
           ([o f a b] (reset! o (f o a b)))
           ([o f a b xs] (reset! o (apply f o a b xs))))))

     (defn use-ref
       ([] (use-ref nil))
       ([init] (specify-atom! (react/useRef init))))

     (defn create-ref [] (specify-atom! (react/createRef)))

     (defn use-sync-external-store [subscribe get-snapshot]
       (react/useSyncExternalStore subscribe get-snapshot))))