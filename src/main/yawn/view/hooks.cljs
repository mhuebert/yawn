(ns yawn.view.hooks
  (:require ["react" :as react]))

;; defined inline to avoid runtime arity disambiguation
(defn use-state [initial-state]
  (react/useState initial-state))

(defn use-effect
  ([f]
   (react/useEffect f))
  ([f deps]
   (react/useEffect f deps)))

(defn use-layout-effect [did-update]
  (react/useLayoutEffect did-update))

(defn use-context [context]
  (react/useContext context))

(defn use-memo [f deps]
  (react/useMemo f deps))

(defn use-callback [f deps]
  (react/useCallback f deps))

(defn use-ref [f deps]
  (react/useRef f deps))