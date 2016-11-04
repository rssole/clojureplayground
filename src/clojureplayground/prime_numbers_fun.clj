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

(def sieve-of-eratosthenes
  (memoize (fn [^Number num] (when (> num 1)
                               (loop [v (into [] (map not (boolean-array num)))
                                      i 2]
                                 (if (<= i (Math/sqrt num))
                                   (if (true? (get v i))
                                     (recur (loop [vv v
                                                   j (* i i)]
                                              (if (<= j num)
                                                (recur (assoc vv j false) (+ j i))
                                                vv))
                                            (inc i))
                                     (recur v (inc i)))
                                   (remove nil?
                                           (map-indexed
                                             (fn [ind value] (when (and (> ind 1) (true? value)) ind)) v))))))))

(defn sieve-of-eratosthenes-revisited
  "Based on a work by @theparanoidtimes"
  [n]
  (let [erathostenes (transient (vec (replicate n true)))]
    (doseq [i (range 2 (Math/sqrt n))
            :let [b (nth erathostenes i)]]
      (if b
        (loop [j (* i i) k 1]
          (if (< j n)
            (do
              (assoc! erathostenes j false)
              (recur (+ (* i i) (* k i)) (inc k)))))))
    (filter some? (map-indexed #(if %2 % nil) (drop 2 (persistent! erathostenes))))))
