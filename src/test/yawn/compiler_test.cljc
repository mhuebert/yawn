(ns yawn.compiler-test
  (:refer-clojure :exclude [compile])
  (:require #?(:clj [yawn.compiler :as compiler :refer [compile]])
            #?(:clj [yawn.infer :as infer])
            [clojure.test :as t :refer [deftest is are]]
            #?(:cljs [yawn.convert :as convert])
            [yawn.shared :refer [*throw-on-interpret*]]
            [yawn.view :as v]
            #?@(:cljs
                [["react" :as react :refer [Fragment] :rename {createElement rce}]
                 ["react-dom/server" :as rdom]]))
  #?(:cljs (:require-macros [yawn.compiler-test :refer [--]])))

(defmacro -- [doc & pairs]
  (if (= 1 (count pairs))
    (first pairs)
    `(do ~@(for [[a b] (->> pairs
                            (remove #{:=>})
                            (partition 2))]
             `(is (= ~a ~b) ~doc)))))

#?(:cljs
   (do
     (defn a-fn [] [:div])
     (defn a-constructor []
       (convert/x [:div]))))

#?(:cljs
   (do
     (def to-string rdom/renderToStaticMarkup)

     (deftest convert-tests
       (is
        (= (let [props {:a 1}]
             (to-string (convert/x [:div props]))
             (to-string (v/x [:div (v/props props)])))
           "<div a=\"1\"></div>")
        "Interpreter can handle dynamic props")

       (are [expr html]
         (= (to-string (convert/x expr))
            (to-string (v/x expr))
            html)

         ;; element
         [:div]
         "<div></div>"

         ;; fn
         (a-fn)
         "<div></div>"

         ;; with classes
         [:div.x.y {:class "z"}]
         "<div class=\"x y z\"></div>"

         ;; dynamic class
         (let [class "c"]
           [:div.a.b {:class class}])
         "<div class=\"a b c\"></div>"

         [:div {:style {:font-size 12}}]
         "<div style=\"font-size:12px\"></div>"

         [:div [a-fn]]
         "<div><div></div></div>"

         [:div (a-fn)]
         "<div><div></div></div>"

         (let [props {:class "c"}]
           [:div.a.b ^:props props])
         "<div class=\"a b c\"></div>"

         (let [props {:style {:font-size 12}}]
           [:div ^:props props])
         "<div style=\"font-size:12px\"></div>"

         (let [c "c"]
           [:div.a {:class ["b" c]}])
         "<div class=\"a b c\"></div>"

         #js["div" nil (v/x #js["div"])]
         "<div><div></div></div>"

         [:... [:div.a] [:div]]
         "<div class=\"a\"></div><div></div>"

         (do [:div])
         "<div></div>"

         (let [props {:style {:font-size 12}}]
           [:div (v/props {:x 1} props)])
         "<div x=\"1\" style=\"font-size:12px\"></div>"


         ;; ^js as metadata on function
         ;; ^:interpret
         ;; ^:el
         ))))

(comment
 (compiler/compile-or-interpret-child
  '(let [props {:a 1}] [:div props])))

#?(:clj
   (deftest compile-tests

     (binding [*throw-on-interpret* true]
       (is
        (thrown? java.lang.Exception
                 (macroexpand '(yawn.infer/maybe-interpret 'a)))
        "Dynamic props must be annotated for the compiler"))


     (-- "DOM tags are compiled to `react/createElement` calls."
         (compile '[:div])
         :=> '(yawn.react/createElement "div" nil))

     (-- "Symbol tags are compiled to function calls"
         (compiler/compile '[my-fn 1 2 3])
         :=> '(yawn.infer/maybe-interpret (my-fn 1 2 3)))

     (-- "a literal map is compiled to a prop object"
         (compile '[:span {:class "a"}])
         :=> '(yawn.react/createElement
               "span"
               (js-obj "className" "a")))

     (-- "a symbol or expression in 1st position is treated as a child element"
         (compile '[:span a])
         :=> '(yawn.react/createElement "span" nil (yawn.infer/maybe-interpret a))

         (compile '[:span (a-fn)])
         :=> '(yawn.react/createElement "span" nil (yawn.infer/maybe-interpret (a-fn))))

     (-- "...unless we tag it with :props metadata"
         (compile '[:span ^:props a])
         :=> '(yawn.react/createElement "span" (yawn.convert/convert-props a)))

     (-- "keys are camelCase'd"
         (compiler/convert-props {:on-click ()})
         :=> {"onClick" ()})

     (-- "class vector is joined at compile time"
         (compiler/convert-props {:class ["b" "c"]})
         :=> {"className" "b c"})

     (-- "class vector may include dynamic elements"
         (compiler/convert-props '{:class ["b" c]})
         :=> '{"className" (clojure.core/str
                            "b "
                            (yawn.compiler/maybe-interpret-class c))})

     (-- "class may be dynamic - with runtime interpretation"
         (compiler/convert-props '{:class x})
         :=> '{"className" (yawn.compiler/maybe-interpret-class x)})

     (-- "classes from tag + props are joined"
         (compile [:h1.b.c {:class "a"}])
         :=> '(yawn.react/createElement "h1" (js-obj "className" "b c a")))

     (-- "joining classes from tag + dynamic class forms"
         (compile '[:div.c1 {:class x}])
         :=> '(yawn.react/createElement
               "div"
               (js-obj
                "className"
                (clojure.core/str "c1 " (yawn.compiler/maybe-interpret-class x))))

         (compile '[:div.c1 {:class ["y" d]}])
         :=> '(yawn.react/createElement
               "div"
               (js-obj
                "className"
                (clojure.core/str "c1 y "
                                  (yawn.compiler/maybe-interpret-class d))))
         )

     (-- "style map is also converted to camel-case"
         (compiler/convert-props '{:style {:font-weight 600}})
         :=> {"style" {"fontWeight" 600}}
         (compiler/convert-props '{:style x})
         :=> '{"style" (yawn.shared/camel-case-keys x)})

     (-- "multiple style maps may be passed (for RN)"
         (compiler/convert-props '{:style [{:font-size 10} x]})
         :=> '{"style" [{"fontSize" 10} (yawn.shared/camel-case-keys x)]})

     (-- "a keyword tag is assumed to map to a DOM element and is compiled to createElement"
         (compile [:div])
         :=> '(yawn.react/createElement "div" nil))

     (-- "a symbol tag is assumed to be a regular function that returns a React element.
       this is compiled to a regular function call - with no special handling of \"props\""
         (compile '[my-fn 1 2 3])
         :=> '(yawn.infer/maybe-interpret (my-fn 1 2 3)))

     (-- "arguments that look like hiccup forms are compiled unless tagged with ^:el"
         (compile '[my-fn [:div]]))
     (compile '[my-fn [other-fn]])
     (compile '[my-fn ^:el [other-fn]])

     ;; to invoke a symbol with createElement (instead of calling it as a function),
     ;; add a ^js hint or use `:>` as the tag
     (-- "invoke a symbol with createElement using :el"
         (compile '[:el my-fn])
         :=> '(yawn.react/createElement my-fn nil))


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Clojure vs React elements

     (-- "invoke a symbol with createElement using :> tag"
         (compile '[:> my-fn])
         :=> '(yawn.react/createElement my-fn nil))

     ;; behind the scenes, `infer/inline?` determines whether a form is already
     ;; a valid React element (in which case, we skip runtime interpretation).
     (def skip? (fn [[x]] (= x (last (compile `[:div ~x])))))
     (-- "primitive strings, numbers, and nil are inlined"
         (skip? ["x"])
         :=> true
         (skip? [1])
         :=> true
         (skip? [nil])
         :=> true
         ;; skip interpretation by adding ^:el or ^js
         (skip? '[^:el (my-fn)])
         :=> true
         (skip? '[^js (my-fn)])
         :=> true)

     (-- "various ways to control interpretation/inlining of children"

         (compile '[:div
                    a ;; maybe-interpret (not as props)
                    ^:interpret b ;; interpret-form
                    ^:el c ;; inline
                    ^js d])
         '(yawn.react/createElement
           "div"
           nil
           (yawn.infer/maybe-interpret a)
           (yawn.convert/x b)
           c
           d))

     (comment
      ;; using ^js allows us to rely on type propagation, eg. and define functions
      ;; whose return values will be inlined. the following is just an example, and
      ;; will not work here in a Clojure repl.
      (defn ^js my-fn [])
      ;; the ^js type can now be inferred at call sites,
      ;; and will not be wrapped with interpretation functions
      .. (my-fn))

     (-- ":... compiles to a react Fragment, children are compiled/interpreted"
         (compile '[:... [:div]])
         '(yawn.react/createElement
           yawn.react/Fragment nil
           (yawn.react/createElement "div" nil)))


     (-- "...with a react key"
         (compile '^{:key "a"} [:...])
         '(yawn.react/createElement yawn.react/Fragment (js-obj "key" "a")))

     (comment
      ;; key as metadata - only on element forms
      (compile ^{:key 1} [:span "x"])
      (compile ^{:key 1} [:span {:foo "bar"} "x"]) ;; with props map
      (compile ^{:key 1} [:span 'd "x"]) ;; with symbol child (unrelated)
      (compile (quote ^{:key 1} [:span ^:props d "x"])) ;; with dynamic props
      (compile '(for [x [1 2 3]]
                  ^{:key (:y x)} [:span x])) ;; dynamic key

      ;; warning - key metadata is ignored because a-symbol is a function
      (compile '^{:key 1} [a-symbol]))

     (-- "compile inner forms"
         (compile '(do [:div]))
         '(do (yawn.react/createElement "div" nil)))

     (-- "ignore inner forms of unknown operators"
         (compile '(hello [:div]))
         '(yawn.infer/maybe-interpret (hello [:div])))

     (-- "Interpreted styles"
         (compiler/literal->js (compiler/convert-props '{a 1}))
         '(js-obj (yawn.shared/camel-case (clojure.core/name a)) 1))

     (-- "Interpreted styles"
         (compiler/literal->js (compiler/convert-props '{:style a}))
         '(js-obj "style" (yawn.shared/camel-case-keys a)))

     (-- "Interpreted style expr"
         (compiler/convert-props '{:style (case type ... ...)})
         '{"style" (yawn.shared/camel-case-keys (case type ... ...))})

     (-- "merge props with dynamic styles"
         (-> (compiler/merge-props '{:style a} '{:style b})
             compiler/convert-props)
         '{"style" (yawn.shared/camel-case-keys (clojure.core/merge a b))})

     (-- "special cases of key renaming"
         (->> (compile [:div {:for 1 ;; special case
                              :class 1 ;; special case
                              :kw-key 1 ;; camelCase
                              :aria-key 1 ;; not camelCase (aria-*)
                              :data-key 1 ;; not camelCase (data-*)
                              "string-key" 1 ;; not camelCase (string)
                              }])
              last
              (filter string?)
              set)
         #{"htmlFor"
           "className"
           "kwKey"
           "aria-key"
           "data-key"
           "string-key"})


     (comment
      ;; interpret
      (compile-or-interpret-child '(hello [:div])) ;; maybe-interpret unknown operator

      (compile-or-interpret-child '(let [a 1] [:div a])) ;; compile inner form, maybe-interpret unknown symbol
      (compile-or-interpret-child '(for [x xs] [:span x])) ;; compile inner return values
      (compile-or-interpret-child '(let [x (for [x [1 2 3]] x)] ;; only wrap-return at top level
                                     x))



      ;; dynamic props with key and ref added at runtime
      (compile '^{:key "k" :ref "r"}
               [:div#id.class ^:props b c])

      (compile '[:div {:data-style (assoc {} :width 10)}]) ;; random props are not coerced to anything
      )))


