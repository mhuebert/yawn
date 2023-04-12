(ns yawn.demo
  (:require [yawn.view :as v]
            [yawn.root :as root]
            [yawn.view.hooks :as hooks]))

(v/defview counter [^string name]
  (let [[x x!] (hooks/use-state (rand-int 100))]
    [:div {:on-click #(x! inc)} x name]))

(defn init []
  (root/create :demo (v/x [counter "clicks"])))