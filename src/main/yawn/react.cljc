(ns yawn.react
  #?(:cljs (:require ["react" :as React])))

;; https://github.com/lilactown/helix/commit/02b1e2072fb8283bd14575bd3686c94d9b15610f
(defn get-react [] #?(:cljs React))

(def Fragment #?(:cljs React/Fragment))
(def Suspense #?(:cljs React/Suspense))
(def createElement #?(:cljs React/createElement))
