(ns adventofcode.mmxvii.solutions-test
  (:require [clojure.test :refer :all]
            [midje.sweet :as ms]
            [adventofcode.mmxvii.solutions :as sol]))

(ms/facts "Day 1 - Captcha facts"
          (sol/captcha "1122") => 3
          (sol/captcha "1111") => 4
          (sol/captcha "1234") => 0
          (sol/captcha "91212129") => 9
          (sol/captcha "777366473277") => 41)
