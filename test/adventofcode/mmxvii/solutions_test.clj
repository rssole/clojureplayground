(ns adventofcode.mmxvii.solutions-test
  (:require [clojure.test :refer :all]
            [midje.sweet :as ms]
            [adventofcode.mmxvii.solutions :as sol]))

(ms/facts "Day 1 - Captcha facts"
          (ms/facts "First star"
                    (sol/captcha "1122") => 3
                    (sol/captcha "1111") => 4
                    (sol/captcha "1234") => 0
                    (sol/captcha "91212129") => 9
                    (sol/captcha "7773664732277") => 36)
          (ms/facts "Second star"
                    (sol/captcha2 "1212") => 6
                    (sol/captcha2 "1221") => 0
                    (sol/captcha2 "123425") => 4
                    (sol/captcha2 "123123") => 12
                    (sol/captcha2 "12131415") => 4))
