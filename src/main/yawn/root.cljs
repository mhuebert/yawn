(ns yawn.root
  (:require ["react-dom/client" :as react:dom]))

(defn render [^js root react-el] (.render root react-el))
(defn create
  ([el] (react:dom/createRoot el))
  ([el content]
   (doto (create el)
     (render content))))
(defn unmount [^js root] (.unmount root))
(defn unmount-soon [^js root] (js/setTimeout #(.unmount root) 0))