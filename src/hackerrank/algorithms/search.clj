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

(defn max-subarray-sum
  "https://www.hackerrank.com/challenges/maximum-subarray-sum?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=3-day-campaign"
  [n m xs]
  (reduce max
          (reduce into []
                  (map
                    (comp deref #(future-call
                                   (fn []
                                     (c/sliding-window-map % (fn [win] (mod (reduce + win) m)) xs n))))
                    (range 1 (inc n))))))

(defn max-subarray-sum-hr []
  "Just wrapped to make it convenient to be run on hackerrank"
  (doseq [_ (range (c/str->int (read-line)))]
    (let [[n m] (c/str->ints (read-line))
          xs (c/str->ints (read-line))]
      (println (max-subarray-sum n m xs)))))

