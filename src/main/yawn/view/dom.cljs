(ns yawn.view.dom
  (:require [applied-science.js-interop :as j]
            ["react" :as react]
            ["react-dom/client" :as rdom]))

(defn find-or-create-element [id]
  (if (or (string? id) (keyword? id))
    (or
     (js/document.getElementById (name id))
     (-> (js/document.createElement "div")
         (j/!set :id (name id))
         (doto (->> js/document.body.append))))
    id))

(defonce !roots (atom {}))

#_(defn ^:dev/after-load render-roots []
  #_(doseq [[_ [^js root form-fn]] @!roots] (.render root (form-fn))))

(defn mount [el form-fn]
  (let [el (find-or-create-element el)
        ^js root (rdom/createRoot el)]
    (swap! !roots assoc el [root form-fn])
    (.render root (form-fn))
    el))

(defn unmount [el]
  (let [el (find-or-create-element el)
        [^js root _] (@!roots el)]
    (when root (.unmount root))
    (swap! !roots dissoc el))
  nil)
