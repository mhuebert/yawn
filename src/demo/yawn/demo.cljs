(ns yawn.demo
  (:require [yawn.view :as v]
            [yawn.view.dom :as dom]
            [yawn.view.hooks :as hooks]))

(v/defview counter [^string name]
  (let [[x x!] (hooks/use-state (rand-int 100))]
    [:div {:on-click #(x! inc)} x name]))

(defn init []
  (dom/mount :demo #(#'counter "clicks")))