(ns yawn.react
  #?(:cljs (:require ["react" :as react]
                     ["react-dom" :as react-dom]
                     [yawn.util :as u])))

#?(:cljs (def Fragment react/Fragment))
#?(:cljs (def Suspense react/Suspense))
#?(:cljs (def ^function createElement react/createElement))
#?(:cljs (defn valid-element? [x] (react/isValidElement x)))
#?(:cljs (defn Portal [target-element content]
           (react-dom/createPortal (u/find-or-create-element target-element) content)))