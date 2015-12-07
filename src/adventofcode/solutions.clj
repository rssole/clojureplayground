(ns adventofcode.solutions
  (:require [clojure.java.io :as io]))


(defn day-x-input [x] (str "adventofcode/day" x "-input"))
(defn slurp-day-input [x] (slurp (io/file (io/resource (day-x-input x)))))
(defn day-input-line-seq [x] (line-seq (io/reader (io/resource (day-x-input x)))))

; day 1
(defn day1 []
  (reduce #(if (= \( %2) (inc %) (dec %)) 0 (slurp-day-input 1)))

; day 2
(defn day2 []
  (let [input (day-input-line-seq 2)
        val-ex (fn [s] (map #(Integer/parseInt %) (clojure.string/split s #"x")))]
    (reduce #(let [[l w h] (val-ex %2) dbl (partial * 2) a (* l w) b (* w h) c (* h l) m (min a b c)]
              (+ % (dbl a) (dbl b) (dbl c) m)) 0 input)))

; day 3
(defn day3 []
  (let [input (slurp-day-input 3)]
    (count
      (second
        (reduce #(let [[[x y] s] % nval (case %2
                                          \^ [x (inc y)]
                                          \v [x (dec y)]
                                          \< [(dec x) y]
                                          \> [(inc x) y])]
                  [nval (conj s nval)])
                [[0 0] #{[0 0]}]
                input)))))

; day 3 part 2... pending :)