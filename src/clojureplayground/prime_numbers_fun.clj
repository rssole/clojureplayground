(ns clojureplayground.prime-numbers-fun
  (:require [clojure.set :as cs]))

; (c) Dejo Josifovic 2016
(def is-prime?
  (memoize
    (fn [^Number num] ^Boolean
    (cond
      (<= num 1) false
      (<= num 3) true
      (or (zero? (mod num 2)) (zero? (mod num 3))) false
      :else (reduce #(if (<= (* %2 %2) num)
                      (reduced true)
                      (if (or (zero? (mod num %2)) (zero? (mod num (+ %2 2))))
                        (reduced false) %))
                    true
                    (range 1 (inc num)))))))

(defn sieve-of-sundaram [n]
  (let [top (inc n)]
    (map (comp inc (partial * 2))
         (sort
           (cs/difference (set (range 1 top))
                          (set (for [i (range 1 top) j (range i top)
                                     :let [x (+ i j (* 2 i j))]
                                     :while (<= x top)]
                                 x)))))))