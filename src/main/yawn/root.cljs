(ns yawn.root
  (:require ["react-dom/client" :as react:dom]
            [applied-science.js-interop :as j]))

(defn find-or-create-element [id]
  (if (or (string? id) (keyword? id))
    (or
     (js/document.getElementById (name id))
     (-> (js/document.createElement "div")
         (j/!set :id (name id))
         (doto (->> js/document.body.append))))
    id))

(defn render [^js root react-el] (.render root react-el))
(defn create
  ([el] (react:dom/createRoot (find-or-create-element el)))
  ([el content]
   (doto (create el)
     (render content))))
(defn unmount [^js root] (.unmount root))
(defn unmount-soon [^js root] (js/setTimeout #(.unmount root) 0))