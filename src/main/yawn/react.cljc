(ns yawn.react
  #?(:cljs (:require ["react" :as React])))

(def Fragment #?(:cljs React/Fragment))
(def Suspense #?(:cljs React/Suspense))

#?(:cljs (def ^function createElement #?(:cljs React/createElement))
   :clj (def createElement))

