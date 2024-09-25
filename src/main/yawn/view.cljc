(ns yawn.view
  (:refer-clojure :exclude [partial])
  (:require #?@(:cljs [["react" :as react]
                       ["react-dom" :refer [createPortal]]
                       ["use-sync-external-store/shim" :refer [useSyncExternalStore]]])
            [yawn.convert #?(:cljs :as :clj :as-alias) convert]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [applied-science.js-interop :as j]
            [yawn.compiler :as compiler]
            [yawn.util :as u])
  #?(:cljs (:require-macros yawn.view)))

(declare el) ;; for type hints

(def ^boolean refresh-enabled? #?(:cljs (exists? js/ReactRefreshRuntime)))

(defmacro x [form]
  (when (:ns &env)
    (compiler/compile &env form)))

(defmacro <> [form]
  (when (:ns &env)
    (compiler/compile &env form)))

(defmacro from-element
  "Creates a view function from an element like :div#id.class or package/ElementName."
  ([kw] (when (:ns &env) (compiler/from-element* kw)))
  ([kw props-or-el] (when (:ns &env) (compiler/from-element* kw props-or-el)))
  ([kw el props] (when (:ns &env) (compiler/from-element* kw el props))))

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

#?(:cljs
   (defn specify-meta! [x m]
     (specify! x
       IMeta
       (-meta [_] m))
     x))

(defn defview:impl
  ([name args] (defview:impl nil name args))
  ([{:as options
     :keys [wrap-expr]
     :or {wrap-expr identity}}
    name args]
   (let [[docstring opts argv & body] (parse-args args string? map?)
         key-fn (:key opts)
         name (vary-meta name merge opts {:tag 'yawn.view/el})
         simple-args (simple-argv argv)
         props-sym (gensym "props")
         names (names name)]
     `(let [refresh?# ~'yawn.view/refresh-enabled?
            ~(:sig names) (when refresh?# (refresh:create-sig))
            ~(:constructor names) (doto (j/fn ~(:constructor names) [~props-sym]
                                          (when refresh?# (~(:sig names)))
                                          ~(wrap-expr `(~@(if (seq argv)
                                                            `(j/let [~argv (j/get ~props-sym :cljs-args)])
                                                            `[do])
                                                        ~@(drop-last body)
                                                        (~'yawn.view/x ~(last body)))))
                                    (j/assoc! :displayName ~(:display names)))]

        (defn ~name
          ~@(when docstring [docstring])
          {:arglists '(~(with-meta argv {:tag 'yawn.view/el}))}
          ~simple-args
          (~'yawn.react/createElement
           ~(:constructor names)
           (j/obj ~@(when key-fn [:key `(~key-fn ~@simple-args)])
                  :cljs-args [~@simple-args])))
        ~@(when opts
            [(list 'specify! name 'IMeta `(~'-meta [~'_] ~opts))])
        (when refresh?#
          (refresh:after ~(:sig names)
                         ~(:constructor names)
                         ~(hook-signature body)
                         ~(:display names)))
        #'~name))))


(defmacro defview [name & args]
  (when (:ns &env)
    (defview:impl name args)))

#?(:clj
   (defmacro classes [v]
     (compiler/compile-classv v)))

#?(:cljs
   (defn classes [v]
     (convert/join-strings " " v)))

#?(:cljs
   (defn portal [^js el react-el]
     (createPortal (convert/x react-el)
                   (u/find-or-create-element el))))

(defn- as-str [c]
  (if (vector? c)
    (str/join " " c)
    c))

(defn- merge-classes [c1 c2]
  (str (as-str c1) " " (as-str c2)))

#?(:cljs
   (do
     (defn ^compiled-props props
       "Merges and converts props. Returns javascript object."
       ([p1 p2 & more]
        (reduce props (props p1 p2) more))
       ([p1 p2]
        (cond-> (convert/convert-props p1)
                p2
                (convert/merge-js-props! (convert/convert-props p2))))
       ([p] (convert/convert-props p)))

     (defn merge-props
       "Merges props. Returns map."
       ([p1 p2 & more]
        (reduce merge-props (merge-props p1 p2) more))
       ([p1 p2]
        (if p2
          (reduce-kv (fn [out k v]
                       (case k :style (update out k merge v)
                               :class (update out k convert/merge-clj-classes v)
                               (assoc out k v))) p1 p2)
          p1)))))

#?(:clj
   (defmacro props [& props]
     (let [convert (fn [props]
                     (if (map? props)
                       (-> props
                           compiler/convert-props
                           compiler/literal->js)
                       `(~'yawn.convert/convert-props ~props)))
           props (map convert props)]
       (with-meta (reduce (fn [p1 p2]
                            `(~'yawn.convert/merge-js-props! ~p1 ~p2))
                          (first props)
                          (rest props))
                  {:tag 'compiled-props}))))
