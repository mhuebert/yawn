(ns yawn.hooks-test
  (:require [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.test :as t :refer [deftest is are async use-fixtures]]
            [yawn.view :as v]
            [yawn.root :as root]
            [yawn.hooks :as h]
            ["react-dom/test-utils" :as tu]))

(set! js/window.IS_REACT_ACT_ENVIRONMENT true)

(def !root (atom nil))

(use-fixtures :each
  {:before #(reset! !root (root/create "root"))
   :after (fn [] (tu/act #(.unmount @!root)))})

(v/defview v-use-state [init call-back]
  (let [!x (h/use-state init)]
    (h/use-effect #(call-back !x))
    [:div (str @!x)]))

(deftest use-tate
  (async done
    (tu/act
     (fn [] (root/render @!root
                         (v-use-state 5
                                      (fn [!state]
                                        (go
                                          (let [root (js/document.querySelector "#root")]
                                            (is (= "5" (.-textContent root)))

                                            (<p! (tu/act #(swap! !state inc)))
                                            (is (= "6" (.-textContent root)))

                                            (done))))))))))

;; is this a good test of use-deps? might be brittle
(v/defview v-use-deps [init call-back]
  (let [!x (h/use-state init)
        !counter (h/use-state 0)]
    (h/use-effect #(call-back !x))
    (h/use-effect #(swap! !counter inc) (h/use-deps @!x))
    [:div (str @!x @!counter)]))

(deftest use-deps
  (async done
    (tu/act
     (fn [] (root/render @!root
                         (v-use-deps "a"
                                     (fn [!state]
                                       (go
                                         (let [root (js/document.querySelector "#root")]
                                           (is (= "a1" (.-textContent root)))

                                           (<p! (tu/act #(reset! !state "b")))
                                           (<p! (tu/act #(reset! !state "b")))
                                           (is (= "b2" (.-textContent root)))

                                           (<p! (tu/act #(reset! !state "c")))
                                           (is (= "c3" (.-textContent root)))

                                           (<p! (tu/act #(reset! !state "a")))
                                           (is (= "a4" (.-textContent root)))

                                           (done))))))))))

(v/defview v-use-state-with-deps [initx inity call-back]
  (let [!x (h/use-state initx)
        !y (h/use-state-with-deps inity (h/use-deps @!x))]
    (h/use-effect #(call-back !x !y))
    [:div (str @!x @!y)]))

(deftest use-state-with-deps
  (async done
    (tu/act
     (fn [] (root/render @!root
                         (v-use-state-with-deps "a" 1
                                                (fn [!x !y]
                                                  (go
                                                    (let [root (js/document.querySelector "#root")]
                                                      (is (= "a1" (.-textContent root)))

                                                      (<p! (tu/act #(swap! !y inc)))
                                                      (is (= "a2" (.-textContent root)))

                                                      (<p! (tu/act #(reset! !x "a")))
                                                      (is (= "a2" (.-textContent root)))

                                                      (<p! (tu/act #(reset! !x "b")))
                                                      (is (= "b1" (.-textContent root)))

                                                      (<p! (tu/act #(swap! !y inc)))
                                                      (is (= "b2" (.-textContent root)))

                                                      (done))))))))))


