(ns amazon.prime-numbers-fun)

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
