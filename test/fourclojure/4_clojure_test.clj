(ns fourclojure.4-clojure-test
  (:require [fourclojure.4-clojure :as c4]
            [clojure.test :refer :all]))

(deftest fourclojure-problems
  (testing "Problem 85 - Power Set"
    (is (= (c4/powerset #{1 2 3})
           #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})))

  (testing "Problem 73 - Analyze a Tic-Tac-Toe Board"
    (is (= nil (c4/tic-tac-toe [[:e :e :e]
                                [:e :e :e]
                                [:e :e :e]])))
    (is (= :x (c4/tic-tac-toe [[:x :e :o]
                               [:x :e :e]
                               [:x :e :o]])))
    (is (= :o (c4/tic-tac-toe [[:e :x :e]
                               [:o :o :o]
                               [:x :e :x]])))
    (is (= nil (c4/tic-tac-toe [[:x :e :o]
                                [:x :x :e]
                                [:o :x :o]])))
    (is (= :x (c4/tic-tac-toe [[:x :e :e]
                               [:o :x :e]
                               [:o :e :x]])))
    (is (= :o (c4/tic-tac-toe [[:x :e :o]
                               [:x :o :e]
                               [:o :e :x]])))
    (is (= nil (c4/tic-tac-toe [[:x :o :x]
                                [:x :o :x]
                                [:o :x :o]])))))
