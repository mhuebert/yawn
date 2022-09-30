(ns yawn.view.refresh-preload
  (:require ["react-refresh/runtime" :as refresh]))

(refresh/injectIntoGlobalHook goog/global)
(goog/exportSymbol "ReactRefreshRuntime" refresh)