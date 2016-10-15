(ns codechef.helper-test
  (:use midje.sweet)
  (:require [codechef.helper :as ch]))

(fact "for radius of incircle 2 there are 5 integer-size sided triangles"
      (ch/chef-and-triangles 2) => {:count 5 :variants [[5 12 13]
                                                        [6 8 10]
                                                        [6 25 29]
                                                        [7 15 20]
                                                        [9 10 17]]})

(facts "odometer should fullfil the following requirements"
       (let [odometer (ch/odometer-revisited 1 10)]
         (fact "given no counter broke limit just last counter increases"
               (odometer [10 10 10 9]) => [10 10 10 10])
         (fact "given counter broke limit, upper counter should be updated accordingly"
               (odometer [5 6 7 10]) => [5 6 8 1]
               (odometer [9 10 10 10]) => [10 1 1 1])
         (fact "given upper limit and all counters reached upper limit all should go back to init"
               (odometer [10 10 10 10]) => [1 1 1 1])))