(ns hackerrank.algorithms.search
  (:require [hackerrank.common :as c]))

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
  (c/with-test-cases-of-array sherlock-and-array))
