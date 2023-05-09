(ns yawn.infer
  (:require [cljs.analyzer :as ana]
            [cljs.env :refer [*compiler*]]
            [yawn.shared]))

(def skip-types '#{number
                   string
                   function
                   js
                   yawn.view/el})

(defn infer-type
  [form env]
  (binding [*compiler* (or *compiler* (atom {}))]
    (ana/infer-tag env (ana/no-warn (ana/analyze env form)))))

;; dev util
(defmacro inferred-type [x]
  (list 'quote (infer-type x &env)))

(defn skipped
  "Returns true if we can skip interpretation"
  [form tag]
  (cond (string? form) form
        (number? form) form
        (nil? form) form
        :else (let [expr-meta (meta form)
                    tag (or (:tag expr-meta) tag)]
                (when (or (contains? skip-types tag)
                          (:el expr-meta))
                  (vary-meta form assoc :tag 'yawn.view/el)))))

(defmacro maybe-interpret
  "Macro that wraps `expr` with interpreter call, if it cannot be skipped based on inferred type."
  [expr]
  (or (skipped expr (infer-type expr &env))
      (do (yawn.shared/warn-on-interpret expr)
          `(~'yawn.convert/x ~expr))))


