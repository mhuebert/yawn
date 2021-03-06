(ns yawn.infer
  (:require [cljs.analyzer :as ana]
            [yawn.env :as env]
            [yawn.util :as util]
            #?(:clj cljs.analyzer.macros))
  #?(:cljs (:require-macros cljs.analyzer.macros)))

(defn infer-type
  [form env]
  (ana/infer-tag env
                 (#?(:clj ana/no-warn :cljs cljs.analyzer.macros/no-warn)
                   (ana/analyze env form))))

;; dev util
(defmacro inferred-type [x]
  (list 'quote (infer-type x &env)))

(defn skip?
  "Returns true if we can skip interpretation"
  ([options form] (skip? options form nil (:skip-types options)))
  ([options form tag] (skip? options form tag (:skip-types options)))
  ([options form tag skip-types]
   (or (string? form)
       (number? form)
       (nil? form)
       (let [expr-meta (meta form)
             tag (or (:tag expr-meta) tag)]
         (boolean (or (contains? skip-types tag)
                      (:inline expr-meta)))))))

(defmacro maybe-interpret
  "Macro that wraps `expr` with interpreter call, if it cannot be skipped based on inferred type."
  [options-sym expr]
  (let [{:keys [skip-types
                warn-on-interpretation?
                throw-on-interpretation?] :as options} @(resolve options-sym)
        tag (infer-type expr &env)]
    (if (skip? options expr tag skip-types)
      expr
      (do (when-not (:interpret (meta expr))
          (when warn-on-interpretation?
            (println (str "WARNING: interpreting form " (pr-str expr)
                          (let [{:keys [line file]} (meta expr)]
                            (when (and line file)
                              (str ", " file ":" line)))
                          (some->> tag (str ", ")))))
          (when throw-on-interpretation?
            (throw (ex-info "Interpreting when not allowed"
                            {:error :throw-on-interpret
                             :form expr}))))
          `(~'yawn.convert/<> ~options-sym ~expr)))))
