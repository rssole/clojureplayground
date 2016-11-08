(ns amazon.interview-preparation-test
  (:require [amazon.interview-preparation :as ip])
  (:use midje.sweet))

(fact "Duplicates should be found by all variants"
      (let [given [4 3 2 7 8 2 3 1]]
        (ip/all-duplicates given) => #{2 3}
        (ip/all-duplicates-revisited given) => [2 3]
        (ip/all-duplicates-leetcode-port given) => '(2 3)))

(fact "Binary search should do exactly that - search for something in sorted array"
      (let [input-range (range 1 11)]
        (ip/binary-search 7 input-range) => 6
        (ip/binary-search 3 input-range) => 2
        (ip/binary-search 15 input-range) => nil))
