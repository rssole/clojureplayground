(ns codingame.helper-test
  (:use [midje.sweet])
  (:require [codingame.helper :as ch]))

(fact "Balanced ternary computers puzzle says:
    Balanced ternary(3 base) is a non-standard positional numeral system.
    In the standard (unbalanced) ternary system, digits have values 0, 1 and 2.
    The digits in the balanced ternary system have values −1, 0, and 1.
    We use letter T to represent -1, so the digits are (T, 0, 1)."
      (fact "8 by balanced ternary computer encoding is 10T")
      (ch/btc-encode 8) => "10T")