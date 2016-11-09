(ns hackerrank.common-test
  (:require [hackerrank.common :as hc])
  (:use midje.sweet))

(fact "String containing space separated numbers should be properly converted"
      (hc/str->ints "1 2 3 4 5 6") => '(1 2 3 4 5 6))
