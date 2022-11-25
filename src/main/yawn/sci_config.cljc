(ns yawn.sci-config
  (:require [applied-science.js-interop]
            [sci.core :as sci]
            [yawn.convert :as convert]
            [yawn.hooks]
            [yawn.react]
            [yawn.view :as v]))

(defn ^:macro defview
  "Defines view function."
  [_ _ name & args]
  (v/defview:impl name args))

(def vns (sci/create-ns 'yawn.view nil))

(def view-namespace
  (merge (select-keys (sci/copy-ns yawn.view vns)
                      '[use-memo
                        use-callback
                        use-effect
                        use-state
                        use-ref
                        use-sync-external-store

                        refresh-enabled?
                        refresh:create-sig
                        refresh:after
                        el])
         {'defview (sci/copy-var defview vns)
          'x (sci/copy-var convert/x vns)}))

(def rns (sci/create-ns 'yawn.react nil))
(def react-namespace (sci/copy-ns yawn.react rns))

(def namespaces {'yawn.view view-namespace
                 'yawn.react react-namespace
                 'yawn.hooks (sci/copy-ns yawn.hooks (sci/create-ns 'yawn.hooks nil))})