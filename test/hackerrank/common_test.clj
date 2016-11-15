(ns hackerrank.common-test
  (:require [hackerrank.common :as hc])
  (:use midje.sweet))

(fact "String containing space separated numbers should be properly converted"
      (hc/str->ints "1 2 3 4 5 6") => '(1 2 3 4 5 6))

(fact "Sliding window of given size sequence should be exactly that"
      (let [sum #(reduce + %)
            test-vec (vec (range 100000))]
        (hc/sliding-window-map 3 sum [1 2 3 4 5 6 7 8] 8) => '(6 9 12 15 18 21)
        (hc/sliding-window-map 2 sum [1 2 3 4 5] 5) => '(3 5 7 9)
        (hc/sliding-window-map 3 sum test-vec 100000) => (map #(reduce + %) (partition 3 1 test-vec))))
