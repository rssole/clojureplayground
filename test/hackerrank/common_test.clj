(ns hackerrank.common-test
  (:require [hackerrank.common :as hc])
  (:use midje.sweet))

(fact "String containing space separated numbers should be properly converted"
      (hc/str->ints "1 2 3 4 5 6") => '(1 2 3 4 5 6))

(let [sum #(reduce + %)
      test-vec (vec (range 100000))]

  (fact "Applying function over sliding window should do exactly that :)"
        (hc/sliding-window-map 3 sum [1 2 3 4 5 6 7 8] 8) => '(6 9 12 15 18 21)
        (hc/sliding-window-map 2 sum [1 2 3 4 5] 5) => '(3 5 7 9)
        (hc/sliding-window-map 3 sum test-vec 100000) => (map #(reduce + %) (partition 3 1 test-vec)))

  (fact "Rising window should start from the beginning and then grow to the end"
        (hc/growing-window-map sum [1 2 3 4 5 6]) => '(1 3 6 10 15 21)))


