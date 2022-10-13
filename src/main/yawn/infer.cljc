(ns yawn.infer
  (:require [cljs.analyzer :as ana]
            cljs.env
            #?(:clj cljs.analyzer.macros))
  #?(:cljs (:require-macros cljs.analyzer.macros)))

(defn infer-type
  [form env]
  (binding [cljs.env/*compiler* (or cljs.env/*compiler* (atom {}))]
    (ana/infer-tag env
                   (#?(:clj ana/no-warn :cljs cljs.analyzer.macros/no-warn)
                    (ana/analyze env form)))))

;; dev util
(defmacro inferred-type [x]
  (list 'quote (infer-type x &env)))

(defn skipped
  "Returns true if we can skip interpretation"
  ([options form] (skipped options form nil (:skip-types options)))
  ([options form tag] (skipped options form tag (:skip-types options)))
  ([options form tag skip-types]
   (cond (string? form) form
         (number? form) form
         (nil? form) form
         :else (let [expr-meta (meta form)
                     tag (or (:tag expr-meta) tag)]
                 (when (or (contains? skip-types tag)
                           (:inline expr-meta))
                   (vary-meta form assoc :tag 'yawn.view/el))))))

(defmacro maybe-interpret
  "Macro that wraps `expr` with interpreter call, if it cannot be skipped based on inferred type."
  [options-sym expr]
  (let [{:keys [skip-types
                warn-on-interpretation?
                throw-on-interpretation?] :as options} @(resolve options-sym)
        tag (infer-type expr &env)]
    (or (skipped options expr tag skip-types)
        (do (when-not (:interpret (meta expr))
              (when warn-on-interpretation?
                (println (str "WARNING: interpreting form " (pr-str expr) ","
                              "tag: " (or tag "not-found") ", "
                              (let [{:keys [line file]} (meta expr)]
                                (when (and line file)
                                  (str ", " file ":" line))))))
              (when throw-on-interpretation?
                (throw (ex-info "Interpreting when not allowed"
                                {:error :throw-on-interpret
                                 :form expr}))))
            `(~'yawn.convert/x ~options-sym ~expr)))))
