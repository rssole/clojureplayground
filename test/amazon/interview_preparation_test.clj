(ns amazon.interview-preparation-test
  (:require [amazon.interview-preparation :as ip])
  (:use midje.sweet))

(fact "Duplicates should be found by all variants"
      (let [given [4 3 2 7 8 2 3 1] expected #{2 3}]
        (ip/all-duplicates given) => expected
        (ip/all-duplicates-revisited given) => expected
        (ip/all-duplicates-leetcode-port given) => expected))

(fact "Binary search should do exactly that - search for something in sorted array"
      (ip/binary-search 7 (range 1 11)) => 6
      (ip/binary-search 3 (range 1 11)) => 2
      (ip/binary-search 15 (range 1 11)) => nil)
