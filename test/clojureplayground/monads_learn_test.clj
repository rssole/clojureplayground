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
               (pure List (f x))))))

    ;Iterchange
    ;The result of applying an Applicative Functor u to the pure value of y
    ; should be the same as taking the Applicative obtained by calling pure
    ; with a function that applies its argument to y and then applying that to u

    ;Haskell way: u <*> pure y = pure ($ y) <*> u
    (testing "Interchange"
      (let [u (pure List #(+ 10 %)) y 50]
        (is (= (<*> u (pure List y))
               (<*> (pure List #(% y)) u)))))))

(deftest monoids-learning
  (let [lmempty (list-monoid)
        pmempty (plus-monoid)]

    (testing "Monoids concept - plus monoid"
      (is (= 0 pmempty))
      (is (= 7 (plus-monoid 3 4)))
      (is (= 9 (reduce plus-monoid [2 3 4]))))

    (testing "Monoids concept - list monoid"
      (is (= '() lmempty))
      (is (= '(1 2 3 4 5 6) (list-monoid [1 2 3] [4 5 6])))
      (is (= '(1 2 3 4 5 6 7 8 9) (reduce list-monoid [[1 2 3] [4 5 6] [7 8 9]]))))

    ;Identity
    ;Applying mappend to mempty and a monoid x should be the same as the original x monoid

    ;Haskell way:
    ; mappend mempty x = x
    ; mappend x mempty = x
    (testing "Identity"
      (is (= 5 (plus-monoid pmempty 5)))
      (is (= '(4) (list-monoid lmempty '(4)))))

    ;Associativity
    ;Applying mappend to a monoid x and the result of applying mappend to the monoids y and z
    ; should be the same as first applying mappend to the monoids x and y and then applying
    ; mappend to the resulting monoid and the monoid z

    ;Haskell way:
    ; mappend x (mappend y z) = mappend (mappend x y) z
    (testing "Associativity"
      (is (= (plus-monoid 1 (plus-monoid 2 3))
             (plus-monoid (plus-monoid 1 2) 3)))
      (is (= (list-monoid '(1) (list-monoid '(2) '(3)))
             (list-monoid (list-monoid '(1) '(2)) '(3)))))))

;Auxiliary functions for monads section
(defn calculate-shipping-rate [address]
  (if (= (:country address) "Australia")
    10.0
    nil))

(defn apply-shipping-costs [order shipping-rate]
  (assoc order :total (+ (:total order) shipping-rate)))

(defn lookup-discount-code [code]
  (if (= code "XMAS2012")
    5.0
    nil))

(defn apply-discount-code [order discount]
  (assoc order :total (- (:total order) discount)))

(defn place [order]
  (let [value (:total order)]
    (prn (str "Off you go! Order total: $" value))
    value))

(def another-order {
                    :items         [{:name "Jalape�o sauce" :price 20.0}]
                    :address       {:country "Australia"}
                    :discount-code "XMAS2012"
                    :total         20.0})


(deftest monads-learning
  (testing "Non-macro usage version"
    (is (= 25.0 (-> another-order
                  ((:bind maybe-monad
                    (fn [order]
                      (-> (calculate-shipping-rate (:address order))
                          ((:bind maybe-monad
                            (fn [shipping-rate]
                              (-> (lookup-discount-code (:discount-code order))
                                  ((:bind maybe-monad
                                    (fn [discount]
                                      ((:return maybe-monad
                                        (-> order
                                            (apply-shipping-costs shipping-rate)
                                            (apply-discount-code discount)
                                            (place))))))))))))))))))))

(defn monad-steps
  ([monad steps expr]
   (if (seq steps)
     (let [fst (first steps)
           snd (second steps)]
       `((:bind ~monad
          (fn [~(symbol fst)]
            (-> ~snd ~(monad-steps monad (subvec steps 2) expr))))))
     expr)))


(defmacro domonad [monad steps expr]
  (let [args (map first (partition 2 steps))
        forms (map second (partition 2 steps))
        new-steps (subvec (vec (interleave (cons nil args) forms)) 2)]
    `(let [m# ~monad]
       (-> ~(second steps)
           ~(monad-steps monad new-steps
                         `((:bind ~monad) (fn [~(symbol (last args))] ((:return ~monad) ~expr))))))))

(deftest monads-learning-ext
  (testing "Macro usage version of previous test, after macros are defined"
    (is (= 25.0 (domonad maybe-monad
                       [order another-order
                        shipping-rate (calculate-shipping-rate (:address order))
                        discount (lookup-discount-code (:discount-code order))]
                       (-> order
                           (apply-shipping-costs shipping-rate)
                           (apply-discount-code discount)
                           (place)))))))