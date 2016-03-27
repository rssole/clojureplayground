(ns clojureplayground.monads-learn-test
  (:require [clojure.test :refer :all]
            [clojureplayground.monads-learn :refer :all])
  (:import (clojureplayground.monads_learn List)))

(deftest functors-learning
  (let [fct (->List [2 3 4])]
    (testing "It should f-map"
      (is (= fct (fmap (->List [1 2 3]) inc))))

    ;Functors must obey to the particular laws

    (testing "Identity"
      ;Haskell way: fmap id functor = id functor
      (is (= (fmap fct identity) (identity fct))))

    (testing "Compositon"
      ;Haskell way: fmap (f . g) functor = fmap f (fmap g functor)
      (let [f #(+ 10 %)
            g #(* 2 %)]
        (is (= (fmap fct (comp f g))
               (-> fct
                   (fmap g)
                   (fmap f))))))))

(deftest applicatives-learning
  (let [fs (->List [#(* 2 %) #(+ 10 %)])
        xs (->List [1 2 3])]

    (testing "It still should f-map 'coz it is still a functor"
      (is (= (->List [true true]) (fmap fs fn?))))

    ;I should never forget that each of applicative functor operations yields again - functor
    (testing "It 'applies' properly - that is how <*> function is called: apply"
      (is (= (->List [2 4 6 11 12 13]) (<*> fs xs))))

    ;Again, applicative functors must obey to the particular laws

    ;Feeding a function f to pure and applying the resulting
    ; Applicative to the Functor v should be the same as
    ; directly mapping f over the Functor v

    ;Haskell way: pure f <*> v = fmap f v
    (testing "Identity"
      (is (= (<*> (pure List inc) xs) (fmap xs inc))))))
