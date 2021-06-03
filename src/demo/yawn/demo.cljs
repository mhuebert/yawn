(ns yawn.demo
  (:require [yawn.view :as v]
            [yawn.view.dom :as dom]
            [yawn.view.react-hooks :as hooks]))

(v/defview counter [name]
  (let [[x x!] (hooks/use-state (rand-int 100))]
    [:div {:on-click #(x! inc)} x name]))

(defn ^:dev/after-load render []
  (dom/render (counter "clicks")
              "demo"))