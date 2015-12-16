(ns adventofcode.solutions
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.core.match :as m])
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

; day 3 part 2...
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

; day 6
(defn- s2int [s] (Integer/parseInt s))

(def ^{:private true} day6-instruction-regex #"(\w+) (?<x0>\d+),(?<y0>\d+) through (?<x1>\d+),(?<y1>\d+)")

(defn- day6-get-instruction [line]
  (let [[[_ action x0 y0 x1 y1]] (re-seq day6-instruction-regex line)]
    [(keyword action) (s2int x0) (s2int y0) (s2int x1) (s2int y1)]))

(def ^{:private true} actions-1 {:off    #(aset-int % %2 %3 0)
                                 :on     #(aset-int % %2 %3 1)
                                 :toggle #(let [val (aget ^ints % %2 %3)
                                                new-val (if (zero? val) 1 0)]
                                           (aset-int % %2 %3 new-val))})

(def ^{:private true} actions-2 {:off    #(let [val (aget ^ints % %2 %3)]
                                           (when (pos? val)
                                             (aset-int % %2 %3 (dec val))))
                                 :on     #(let [val (aget ^ints % %2 %3)]
                                           (aset-int % %2 %3 (inc val)))
                                 :toggle #(let [val (aget ^ints % %2 %3)]
                                           (aset-int % %2 %3 (+ 2 val)))})

(defn- day6-stub [actions rf]
  (let [input (day-input-line-seq 6)
        board (make-array Integer/TYPE 1000 1000)]
    (doseq [l input]
      (let [[action x0 y0 x1 y1] (day6-get-instruction l)
            x-rng (range x0 (inc x1))
            y-rng (range y0 (inc y1))
            act (action actions)]
        (doseq [x x-rng y y-rng]
          (act board x y))))
    (reduce rf 0 board)))

(defn day6 []
  (day6-stub actions-1 #(+ % (count (filter (fn [x] (= 1 x)) %2)))))

(defn day6-part2 []
  (day6-stub actions-2 #(+ % (reduce + 0 %2))))

; day 7
(def day7-get-instruction-regex #"(((\w+|\d+) )?(\w+) )?(\d+|\w+) -> (\w+)")

(defn day7-instr-parts [line]
  (first (re-seq day7-get-instruction-regex line)))

(defn is-integer
  "Very basic check just for purpose of day 7"
  [s]
  (some? (re-matches #"\d+" s)))

(defn to-int [s]
  (when (and (not (st/blank? s)) (is-integer s))
    (s2int s)))

(defn day-7-get-instruction [line]
  (let [[_ _ _ lw oper rw target] (day7-instr-parts line)
        lw-num (to-int lw) rw-num (to-int rw)]
    {:lwire lw :lw-num lw-num :oper (keyword oper) :rwire rw :rw-num rw-num :target target}))

(def operations {:RSHIFT #(bit-shift-right % %2)
                 :LSHIFT #(bit-shift-left % %2)
                 :NOT    bit-not
                 :OR     bit-or
                 :AND    bit-and})

(defn day-7-reducing-f [m instr]
  (let [{target :target} instr]
    (if (contains? m target)
      m
      (m/match [instr]
               ; matches below probably could be simplified by means of "guards"
               [{:oper nil :lwire nil :rwire rw :rw-num nil}] (if (contains? m rw)
                                                                (assoc m target (get m rw))
                                                                m)
               [{:oper nil :rw-num rn}] (assoc m target rn)
               [{:oper o :lwire nil :rwire rw :rw-num nil}] (if (contains? m rw)
                                                              (assoc m target ((o operations) (get m rw)))
                                                              m)
               [{:oper o :lwire nil :rwire _ :rw-num rn}] (assoc m target ((o operations) rn))
               [{:oper o :lwire lw :lw-num nil :rwire rw :rw-num nil}] (if (and (contains? m lw) (contains? m rw))
                                                                         (assoc m target ((o operations) (get m lw) (get m rw)))
                                                                         m)
               [{:oper o :lwire _ :lw-num ln :rwire rw :rw-num nil}] (if (contains? m rw)
                                                                       (assoc m target ((o operations) ln (get m rw)))
                                                                       m)
               [{:oper o :lwire lw :lw-num nil :rwire _ :rw-num rn}] (if (contains? m lw)
                                                                       (assoc m target ((o operations) (get m lw) rn))
                                                                       m)
               :else m))))

(defn day7 []
  (let [input (map day-7-get-instruction (day-input-line-seq 7))]
    (loop [resolved {}]
      (let [new-resolved (reduce day-7-reducing-f resolved input)]
        (if (contains? resolved "a")
          resolved
          (recur new-resolved))))))

;for part 2 just place value retrieved for wire a to wire b in input for day 7:)

