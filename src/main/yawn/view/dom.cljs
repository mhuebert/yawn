(ns yawn.view.dom
  (:require [applied-science.js-interop :as j]
            ["react" :as react]
            ["react-dom" :as rdom]))

(defn find-or-create-element [id]
  (or
   (js/document.getElementById id)
   (-> (js/document.createElement "div")
       (j/!set :id id)
       (doto (->> js/document.body.append)))))

(defn render [view id-or-element]
  (rdom/render view (cond-> id-or-element
                            (string? id-or-element)
                            find-or-create-element)))