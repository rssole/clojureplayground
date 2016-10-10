(ns codechef.helper-test
  (:use midje.sweet)
  (:require [codechef.helper :as ch]))

(fact "for radius of incircle 2 there are 5 integer-size sided triangles"
      (ch/chef-and-triangles 2) => {:count 5 :variants [[5 12 13]
                                                        [6 8 10]
                                                        [6 25 29]
                                                        [7 15 20]
                                                        [9 10 17]]})