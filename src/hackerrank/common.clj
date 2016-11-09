(ns hackerrank.common
  (:require [clojure.string :as cs]))

(defn str->int [s]
  (Integer/parseInt s))

(defn str->ints
  "Turns space separated string of numbers into sequence of them"
  [s]
  (map str->int (cs/split s #" ")))

(defn with-test-cases-of-array [f]
  (let [cases-count (str->int (read-line))
        outcomes (repeatedly cases-count (fn []
                        (let [_not-needed (read-line)
                              xs (str->ints (read-line))]
                          (f xs))))]
    (doseq [o outcomes]
      (if o
        (println "YES")
        (println "NO")))))

(defn sherlock-and-array [xs]
  (if (< (count xs) 3)
    nil
    (letfn [(sum [ys] (apply + ys))]
      (some identity (map-indexed (fn [i _]
                                    (let [[l [_ & r]] (split-at i xs)]
                                      (when (= (sum l) (sum r))
                                        i))) xs)))))

(defn sherlock-and-array-hr
  "Prepared for HackerRank submission :) more or less... :)))"
  []
  (with-test-cases-of-array sherlock-and-array))

(sherlock-and-array-hr)
