(ns hackerrank.common
  (:require [clojure.string :as cs]))

(defn str->int [s]
  (Integer/parseInt s))

(defn str->ints
  "Turns space separated string of numbers into sequence of them"
  [s]
  (mapv str->int (cs/split s #" ")))

(defn with-test-cases-of-array [f]
  (let [cases-count (str->int (read-line))
        outcomes (repeatedly
                   cases-count
                   (fn []
                     (let [_ (str->int (read-line))
                           xs (str->ints (read-line))]
                       (f xs))))]
    (doseq [o outcomes]
      (if o
        (println "YES")
        (println "NO")))))

(defn sliding-window-map
  "Applies given function f over sliding window of given win-size took from xs of xs-size.
  This function is aimed for solving HackerRank problem where peformance is a must and not
  to be pure idiomatic Clojure function"
  [win-size f ^clojure.lang.PersistentVector xs xs-size]
  (loop [i 0 acc []]
    (if (<= (+ i win-size) xs-size)
      (recur (inc i) (conj acc (f (subvec xs i (+ i win-size)))))
      acc)))
