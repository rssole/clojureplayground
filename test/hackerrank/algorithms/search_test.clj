(ns hackerrank.algorithms.search-test
  (:require [hackerrank.algorithms.search :as has])
  (:use midje.sweet))

(facts "Sherlock and Array:
        If there exists an element in the array such that the sum of the elements on its
        left is equal to the sum of the elements on its right"

       (fact "For empty or two elements array, nil should be returned..."
             (has/sherlock-and-array []) => nil
             (has/sherlock-and-array [1 2]) => nil)

       (fact "...here, requirement is satisfied"
             (has/sherlock-and-array [1]) => 0
             (has/sherlock-and-array [1 2 3 3]) => 2
             (has/sherlock-and-array [1 4 1]) => 1)

       (fact "...here, again, there is no matching element."
             (has/sherlock-and-array [1 2 3]) => nil))


