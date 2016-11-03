(ns amazon.interview-preparation-test
  (:require [amazon.interview-preparation :as ip])
  (:use midje.sweet))

(fact "Duplicates should be found"
      (ip/all-duplicates [4 3 2 7 8 2 3 1]) => #{2 3})
