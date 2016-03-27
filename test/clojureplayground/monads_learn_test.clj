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

    ;Identity
    ;Feeding a function f to pure and applying the resulting
    ; Applicative to the Functor v should be the same as
    ; directly mapping f over the Functor v

    ;Haskell way: pure f <*> v = fmap f v
    (testing "Identity"
      (is (= (<*> (pure List inc) xs) (fmap xs inc))))

    ;Composition
    ;The result of applying an Applicative Functor
    ; that yields the function composition operator to the Applicative u,
    ; then apply the resulting Functor to v and finally applying that result
    ; to the final Applicative w should be the same as applying v to w and
    ; then applying u to the resulting Applicative.

    ;Haskell way: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    (testing "Composition"
      (let [u (->List [#(* 2 %)])
            v (->List [#(+ 10 %)])
            w xs]                                           ;just re-using same functor with other name to match haskell way above
        (is (= (-> (pure List #(partial comp %))
                   (<*> u)
                   (<*> v)
                   (<*> w))
               (<*> u (<*> v w))))))

    ;Homomorphism
    ;The result of applying the pure value of f to the pure value of x
    ; should be the same as applying f directly to x and then feeding that into pure.

    ;Haskell way: pure f <*> pure x = pure (f x)
    (testing "Homomorphism"
      (let [f #(* 2 %) x 10]
        ;Original example favors thread first macro -> but I think that
        ;in this case this explicit version makes more clear what is going on
        (is (= (<*> (pure List f) (pure List x))
               (pure List (f x))))))))
