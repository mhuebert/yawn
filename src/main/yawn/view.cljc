(ns yawn.view
  (:require [clojure.string :as str]
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

#?(:cljs
   (do
     (def ^boolean refresh-enabled?
       (and goog/DEBUG (exists? js/ReactRefreshRuntime)))))


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

  (defmacro <> [form]
    (c/compile hiccup-opts form))

  (defmacro defview [name & args]
    (let [[docstring opts argv & body] (parse-args args string? map?)
          qualified-name (str *ns* "/" name)
          refresh-signature-sym (gensym (str name "-signature"))
          constructor-sym (symbol (str name "-fn"))
          key-fn (:key opts)
          key-fn-sym (gensym (str name "-keyfn"))
          name (vary-meta name assoc :tag 'js)
          simple-args (simple-argv argv)]
      `(let [~refresh-signature-sym (when ~'yawn.view/refresh-enabled?
                                      (js/ReactRefreshRuntime.createSignatureFunctionForTransform))
             ~constructor-sym (-> (j/fn ~constructor-sym    ;; name
                                    [^:js {~(if (= 1 (count argv))
                                              (vary-meta (first argv) assoc :clj true)
                                              (with-meta argv {:js/shallow true})) :children :as p#}]
                                    (when ~'yawn.view/refresh-enabled? (~refresh-signature-sym))
                                    ~@(drop-last body)
                                    (c/<> hiccup-opts ~(last body)))
                                  (j/!set :displayName ~qualified-name))
             ~@(when key-fn [key-fn-sym key-fn])]
         (defn ~name
           ~@(when docstring [docstring])
           {:arglists (~argv)}
           ~simple-args
           (~@(:create-element-compile hiccup-opts)
             ~constructor-sym
             ~(when key-fn `(j/obj :key (~key-fn-sym ~(first argv))))
             ~@simple-args))
         (when ~'yawn.view/refresh-enabled?
           ;; type, key, forceReset, getCustomHooks
           (~refresh-signature-sym ~name ~(hook-signature body) nil nil)
           (.register js/ReactRefreshRuntime ~name ~qualified-name)
           (js/ReactRefreshRuntime.performReactRefresh))
         #'~name))))

