(ns adventofcode.solutions
  (:require [clojure.java.io :as io])
  (:import (java.security MessageDigest)))


(defn day-x-input [x] (str "adventofcode/day" x "-input"))
(defn slurp-day-input [x] (slurp (io/file (io/resource (day-x-input x)))))
(defn day-input-line-seq [x] (line-seq (io/reader (io/resource (day-x-input x)))))

; day 1
(defn day1 []
  (reduce #(if (= \( %2) (inc %) (dec %)) 0 (slurp-day-input 1)))

(defn day1-part2 []
  (reduce #(let [[floor cnt] % new-floor (if (= \( %2) (inc floor) (dec floor)) val [new-floor (inc cnt)]]
            (if (= -1 new-floor)
              (reduced val)
              val)) [0 0] (slurp-day-input 1)))

; day 2
(defn -day2 [f-lwh]
  (let [input (day-input-line-seq 2)
        val-ex (fn [s] (map #(Integer/parseInt %) (clojure.string/split s #"x")))]
    (reduce #(let [[l w h] (val-ex %2)]
              (+ % (f-lwh l w h))) 0 input)))

(defn day2 []
  (-day2 #(let [a (* % %2) b (* %2 %3) c (* % %3) m (min a b c)]
           (+ (* 2 a) (* 2 b) (* 2 c) m))))

(defn day2-part2 []
  (-day2 #(let [[a b] (take 2 (sort [% %2 %3]))]
           (+ (* 2 a) (* 2 b) (* % %2 %3)))))

; day 3
(def -move #(case %
             \^ [%2 (inc %3)]
             \v [%2 (dec %3)]
             \< [(dec %2) %3]
             \> [(inc %2) %3]))

(defn day3 []
  (let [input (slurp-day-input 3)]
    (count
      (second
        (reduce #(let [[[x y] s] % nval (-move %2 x y)]
                  [nval (conj s nval)])
                [[0 0] #{[0 0]}]
                input)))))

; day 3 part 2... pending :)
(defn day3-part2 []
  (let [input (partition 2 (slurp-day-input 3))]
    (count
      (second
        (reduce #(let [[[[x1 y1] [x2 y2]] s] % nval (-move (first %2) x1 y1) m-nval (-move (second %2) x2 y2)]
                  [[nval m-nval] (conj s nval m-nval)])
                [[[0 0] [0 0]] #{[0 0]}]
                input)))))

; day 4
(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(fn day-4-both-parts []
  (let [input "iwrupvqb"]
    (reduce #(let [m5 (md5 (str input %2))]
              (if (.startsWith m5 "00000")                  ;part 2 just asks for one more zero :)
                (reduced %2)
                %2))
            (range))))

; day 5
(defn day5 []
  (let [input (day-input-line-seq 5)]
    (count
      (reduce (fn [a i] (let [vcnt (count (filter #(#{\a \e \i \o \u} %) i))
                              has-dbl (some #(apply = %) (partition 2 1 i))
                              allowed (not (some true? (map #(.contains i %) ["ab", "cd", "pq", "xy"])))]
                          (if (and has-dbl (>= vcnt 3) allowed)
                            (conj a i)
                            a)))
              []
              input))))

; day 5 - part 2
(defn day5-part2 []
  (let [input (day-input-line-seq 5)]
    (count
      (reduce (fn [a i] (let [has2pairs (re-find #"((\w)(\w)).*(\2\3)" i)
                              rletter-w-1-between (re-find #"(\w)\w\1" i)]
                          (if (and has2pairs rletter-w-1-between)
                            (conj a i)
                            a)))
              []
              input))))

