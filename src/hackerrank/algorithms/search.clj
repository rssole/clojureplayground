(ns hackerrank.algorithms.search
  (:require [hackerrank.common :as c]))

(defn sherlock-and-array [xs]
  (let [sum (fn [ys] (apply + ys))
        out (reduce (fn [[i ls rs] x]
                      (let [nls (+ ls x)
                            nrs (- rs x)]
                        (if (= ls nrs) (reduced i) [(inc i) nls nrs])))
                    [0 0 (sum xs)]
                    xs)]
    (if (vector? out) nil out)))

(defn sherlock-and-array-hr
  "Prepared for HackerRank submission :) more or less... :)))"
  []
  (c/with-test-cases-of-array sherlock-and-array))

