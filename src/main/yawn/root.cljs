(ns yawn.root
  (:require ["react-dom" :refer [createPortal]]
            ["react-dom/client" :refer [createRoot]]
            [yawn.util :as u]))

(defn render [^js root react-el] (.render root react-el))
(defn create
  ([el] (createRoot (u/find-or-create-element el)))
  ([el content]
   (doto (create el)
     (render content))))
(defn unmount [^js root] (.unmount root))
(defn unmount-soon [^js root] (js/setTimeout #(.unmount root) 0))