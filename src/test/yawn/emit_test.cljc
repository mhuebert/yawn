(ns yawn.emit-test
  (:require [yawn.compiler :as c]
            [yawn.convert #?(:cljs :as :clj :as-alias) convert]
            #?(:cljs [yawn.react]))
  #?(:cljs (:require-macros [yawn.emit-test :refer [emit]])))

(defmacro emit [label expr]
  `(do (~'js/console.log "START" ~label "------------------------")
       (def ^:export ~(gensym (str "a" label)) ~expr)
       (~'js/console.log "END  " ~label "------------------------")))

#?(:cljs
   (do
     (emit 1 (c/x [:div]))

     (emit 2 (c/x [:div.a {:class "b"}]))

     (let [B "b"
           D "d"]
       (emit 3 (c/x [:div.a {:class [B "c" D]}])))

     (emit 4 (c/x [:div {:style {:font-size 12}}]))

     (emit 5 (yawn.react/createElement "div"))

     ))

(comment
 (convert/x [:div {:on-click #()}])
 )

