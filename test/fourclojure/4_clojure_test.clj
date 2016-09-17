(ns fourclojure.4-clojure-test
  (:require [fourclojure.4-clojure :as c4]
            [clojure.test :refer :all]))

(deftest fourclojure-problems
  (testing "Problem 85 - Power Set"
    (is (= (c4/powerset #{1 2 3})
           #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))))
