(ns fpinscala.gettingstarted)

;exercise 2.1
;mine
(defn fib [n]
  (letfn [(do-fib [x]
                  (loop [xs [0 1] cnt 0]
                    (if (>= cnt (dec x))
                      (if (= x cnt) (first xs) (second xs))
                      ;(if (> x cnt) (second xs) (first xs))
                      (recur [(last xs) (apply + xs)] (inc cnt)))))]
    (do-fib n)))
;fpinscala :) haa-haa
(defn fib-fpins [n]
  (letfn [(do-fib [n prev curr]
                  (if (= n 0)
                    prev
                    (recur (dec n) curr (+ prev curr))))]
    (do-fib n 0 1)))
;-----------------------------------------------------------------------

;exercise 2.2
;mine
(defn is-sorted [xs f]
  (loop [lxs xs acc true]
    (if (< 2 (count lxs))
      acc
      (recur (rest lxs) (and acc (f (first lxs) (second lxs)))))))
;fpinscala
(defn is-sorted-fpins [xs f]
  (letfn [(go [n]
              (if (>= n (dec (count xs)))
                true
                (if (f (nth xs n) (nth xs (inc n)))
                  false
                  (go (inc n)))))]
    (go 0)))
;-----------------------------------------------------------------------

;exercise 2.3
;mine
(defn curry [f x]
  (fn
    ([a b] (f a b))
    ([y] (f x y))))
; this would be cheating as turning single arity into multiple arity function is not currying :P
;fpinscala
(defn curry-fpins [f x]
  (fn [y] (f x y)))
;however, this way it makes no uncurry possible in clojure
;-----------------------------------------------------------------------

;exercise 2.4
;mine
(defn uncurry [f]
  (fn [x y] (f x y)))
;fpinscala
;no easy way to do in clojure as it does not support multiple arguments list
;-----------------------------------------------------------------------

;exercise 2.5
;mine
(defn compose [f g]
  (fn [arg]
    (f (g arg))))
;fpinscala
;same :)
;-----------------------------------------------------------------------