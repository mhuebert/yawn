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

(defmacro x
  "Converts `form` to React element"
  [form]
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

(defn defview:impl [name args]
  (let [[docstring opts argv & body] (parse-args args string? map?)
        key-fn (:key opts)
        name (vary-meta name assoc :tag `el)
        simple-args (simple-argv argv)
        props-sym (gensym "props")]
    `(let [~@(refresh:bindings name)
           ~(sym:ctor name) (doto (j/fn ~(sym:ctor name) [~props-sym]
                                    (~@(if (seq argv)
                                         `(j/let [~argv (j/get ~props-sym :cljs-args)])
                                         `[do])
                                     ~(refresh:inner name)
                                     ~@(drop-last body)
                                     (x ~(last body))))
                              (j/assoc! :displayName ~(qname name)))]

       (defn ~name
         ~@(when docstring [docstring])
         {:arglists '(~(with-meta argv {:tag 'yawn.view/el}))}
         ~simple-args
         (~'yawn.react/createElement
          ~(sym:ctor name)
          (j/obj ~@(when key-fn [:key `(~key-fn ~@simple-args)])
                 :cljs-args [~@simple-args])))
       ~(refresh:after name body)
       #'~name)))

(defmacro defview [name & args]
  (defview:impl name args))


;; React API access
#?(:cljs
   (do

     #_(deftype WrappedRef [ref]
         IDeref
         (-deref [^js this] (j/!get ref :current))
         IReset
         (-reset! [^js this new-value] (j/!set ref :current new-value))
         ISwap
         (-swap! [this f] (j/!set ref :current (f @this)))
         (-swap! [this f a] (j/!set ref :current (f @this a)))
         (-swap! [this f a b] (j/!set ref :current (f @this a b)))
         (-swap! [this f a b xs] (j/!set ref :current (apply f @this a b xs))))

     (deftype WrappedState [st]
       IIndexed
       (-nth [coll i] (aget st i))
       (-nth [coll i nf] (or (aget st i) nf))
       IDeref
       (-deref [^js this] (aget st 0))
       IReset
       (-reset! [^js this new-value] ((aget st 1) (constantly new-value)))
       ISwap
       (-swap! [this f] ((aget st 1) f))
       (-swap! [this f a] ((aget st 1) #(f % a)))
       (-swap! [this f a b] ((aget st 1) #(f % a b)))
       (-swap! [this f a b xs] ((aget st 1) #(apply f % a b xs))))

     (defn- as-array [x] (cond-> x (not (array? x)) to-array))

     (defn use-memo
       "React hook: useMemo. Defaults to an empty `deps` array."
       ([f] (react/useMemo f #js[]))
       ([f deps] (react/useMemo f (as-array deps))))

     (defn use-callback
       "React hook: useCallback. Defaults to an empty `deps` array."
       ([x] (use-callback x #js[]))
       ([x deps] (react/useCallback x (to-array deps))))

     (defn- wrap-effect
       ;; utility for wrapping function to return `js/undefined` for non-functions
       [f] #(let [v (f)] (if (fn? v) v js/undefined)))

     (defn use-effect
       "React hook: useEffect. Defaults to an empty `deps` array.
        Wraps `f` to return js/undefined for any non-function value."
       ([f] (use-effect (wrap-effect f) #js[]))
       ([f deps] (react/useEffect (wrap-effect f) (as-array deps))))

     (defn use-state
       "React hook: useState."
       [init]
       (WrappedState. (react/useState init)))

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

     (defn use-sync-external-store [subscribe get-snapshot]
       (useSyncExternalStore subscribe get-snapshot))))