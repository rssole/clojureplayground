(ns adventofcode.solutions
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.core.match :as m])
  (:import (java.security MessageDigest)))


(defn- day-x-input [x] (str "adventofcode/day" x "-input"))
;(defn day-x-input [x] (str "adventofcode/day" x "-input-test"))
;(defn day-x-input [x] (str "adventofcode/temp.txt"))
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
(defn- dday2 [f-lwh]
  (let [input (day-input-line-seq 2)
        val-ex (fn [s] (map #(Integer/parseInt %) (clojure.string/split s #"x")))]
    (reduce #(let [[l w h] (val-ex %2)]
              (+ % (f-lwh l w h))) 0 input)))

(defn day2 []
  (dday2 #(let [a (* % %2) b (* %2 %3) c (* % %3) m (min a b c)]
           (+ (* 2 a) (* 2 b) (* 2 c) m))))

(defn day2-part2 []
  (dday2 #(let [[a b] (take 2 (sort [% %2 %3]))]
           (+ (* 2 a) (* 2 b) (* % %2 %3)))))

; day 3
(defn- move #(case %
             \^ [%2 (inc %3)]
             \v [%2 (dec %3)]
             \< [(dec %2) %3]
             \> [(inc %2) %3]))

(defn day3 []
  (let [input (slurp-day-input 3)]
    (count
      (second
        (reduce #(let [[[x y] s] % nval (move %2 x y)]
                  [nval (conj s nval)])
                [[0 0] #{[0 0]}]
                input)))))

; day 3 part 2...
(defn day3-part2 []
  (let [input (partition 2 (slurp-day-input 3))]
    (count
      (second
        (reduce #(let [[[[x1 y1] [x2 y2]] s] % nval (move (first %2) x1 y1) m-nval (move (second %2) x2 y2)]
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

(defn day-4-both-parts []
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
(defn- s->int [s] (Integer/parseInt s))

(def ^{:private true} day6-instruction-regex #"(\w+) (?<x0>\d+),(?<y0>\d+) through (?<x1>\d+),(?<y1>\d+)")

(defn- day6-get-instruction [line]
  (let [[[_ action x0 y0 x1 y1]] (re-seq day6-instruction-regex line)]
    [(keyword action) (s->int x0) (s->int y0) (s->int x1) (s->int y1)]))

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
    (s->int s)))

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

(def filters [#"(?<dblslsh>\\\\)"
              #"(?<sinq>\\\")"
              #"(?<ascii>\\x[0-9a-f]{2})"])

(defn- actual-chars
  "Counts actual number of chars in escaped string"
  [str]
  (let [cnt (count str)
        content (subs str 1 (dec cnt))]
    (loop [c content acc 0]
      (if-let [ch (first c)]
        (case ch
          \\ (let [nx (first (rest c))]
               (case nx
                 (\\ \") (recur (drop 2 c) (inc acc))
                 \x (recur (drop 4 c) (inc acc))))
          (recur (drop 1 c) (inc acc)))
        [cnt acc]))))

(defn day8 []
  (let [input (day-input-line-seq 8)]
    (reduce
      (fn [acc line]
        (let [[code mem] (actual-chars line)]
          (+ acc (- code mem))))
      0
      input)))

(def filters [#"(?<dblslsh>\\\\)"
              #"(?<ascii>\\x[0-9a-f]{2})"
              #"(?<sinq>\\\")"])

(defn- d8-resolver [acc line]
  (let [cnt (count line)
        content (.substring line 1 (dec cnt))
        last-s (reduce #(st/replace % %2 "#") content filters)
        diff (- cnt (count last-s))]
    (+ acc diff)))

(defn day8-rex []
  (let [input (day-input-line-seq 8)]
    (reduce d8-resolver 0 input)))

(defn- enc-chars
  "Counts number of chars should string be re-encoded"
  [str]
  (let [cnt (count str)
        enc (reduce #(case %2
                      (\\ \") (+ 2 %)
                      (inc %)) 2 str)]
    [cnt enc]))

(defn day8-p2 []
  (let [input (day-input-line-seq 8)]
    (reduce
      (fn [acc line]
        (let [[code enc] (enc-chars line)]
          (+ acc (- enc code))))
      0
      input)))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn day9
  "Provide min/max as comp-f parameters for parts 1 and 2 respectively"
  [comp-f]
  (let [input (day-input-line-seq 9)
        distances (into {} (map #(let [[[_ from to dist]] (re-seq #"^(\w+) to (\w+) = (\d+)$" %)]
                                  [#{from to} (s->int dist)]) input))
        places (reduce #(let [[endpoints _] %2]
                         (conj % (first endpoints) (last endpoints))) #{} distances)
        all-routes (permutations places)
        route-len (fn [r] (reduce #(let [dist (distances %2)]
                                    (+ dist %)) 0 (map set (partition 2 1 r))))]
    (reduce #(comp-f % (route-len %2))
            (route-len (first all-routes))
            (rest all-routes))))

(defn day10
  "My input was '1113222113' and 40 and 50 iterations for parts 1 and 2 respectively"
  [input iterations]
  (count
    (nth
      (iterate
        (fn [x]
          (clojure.string/join
            (map #(str (count %) (first %)) (partition-by identity x))))
        input)
      iterations)))

;day 11
;just building blocks
(defn- no-iol? [ch-seq]
  (not-any? #(#{105 108 111} %) ch-seq))

(defn- s->ch-seq
  "Turns string into sequence of char (integer) codes"
  [s]
  (vec (map int s)))

(defn- ch-seq->str
  "Char sequence (ch here is 8bit int actually)"
  [chs]
  (apply str (map char chs)))

(defn- has-distinct-pairs?
  "Determines if sequence contains two distinct pairs (pairs consist of different chars)"
  [xs]
  (let [[one two] (take 2 (filter
                            #(= 2 (count %))
                            (partition-by identity xs)))]
    (and one two (not= (first one) (first two)))))

(defn- has-inc-of-three?
  "Determines if provided input sequence containes increasing straight of at least three letters"
  [xs]
  (= 3 (count
         (reduce #(if (= (dec %2) (last %))
                   (let [med (conj % %2)]
                     (if (= 3 (count med))
                       (reduced med)
                       med))
                   [%2]) [(first xs)] (rest xs)))))

;naive inefficient attempt
;(defn day11 [input]
;  (let [a0 (first (s->ch-seq input))]
;    (for [a (range (int a0) 123)
;          b (range 97 123)
;          c (range 97 123)
;          d (range 97 123)
;          e (range 97 123)
;          f (range 97 123)
;          g (range 97 123)
;          h (range 97 123)
;          :let [cs [a b c d e f g h]]
;          :when (and (no-iol? cs)
;                     (has-inc-of-three? cs)
;                     (has-distinct-pairs? cs)
;                     (pos? (compare (ch-seq->str cs) input)))]
;      cs)))

;quite enough efficient solution
(defn day11 [input]
  (reduce #(if (and
                 (has-inc-of-three? %2)
                 (no-iol? %2)
                 (has-distinct-pairs? %2))
            (reduced (ch-seq->str %2)))
          (iterate (fn [x]
                     (if (= 122 (last x))
                       (let [cnt (count (take-while #(= % 122) (reverse x)))]
                         (vec (map-indexed #(let [pos (- 7 cnt)]
                                             (cond
                                               (= pos %) (inc %2)
                                               (< % pos) %2
                                               (> % pos) 97))
                                           x)))
                       (assoc x 7 (inc (last x))))) (s->ch-seq input))))

;day 12
;part 1
;simply:
(reduce + (map #(s->int %) (re-seq #"\-?\d+" (slurp-day-input 12))))

;part 2 a "bit" more involved :)
(defn- move-on-balance-and-current
  "a is balance and current position map, fb is function to apply against balance"
  [a fb]
  (-> a
      (update :current inc)
      (update :balance fb)))

(defn- where-is-first-balanced
  "Will find first occurence of particular char c which is balanced with it's counterpart ic"
  [input c ic]
  (reduce (fn [a x]
            (condp = x                                      ;why not use "case"? Because case requires compile-time constants!!!
              ic (move-on-balance-and-current a inc)
              c (if (pos? (:balance a))
                  (move-on-balance-and-current a dec)
                  (reduced a))
              (move-on-balance-and-current a identity)))
          {:balance 0 :current 0}
          input))

(defn- where-jsobj-starts [s]
  (- (count s) (:current (where-is-first-balanced (reverse s) \{ \}))))
(defn- where-jsobj-ends [s]
  (:current (where-is-first-balanced s \} \{)))

(defn- rm-red-jsobj
  "Removes single JS object containing property with value 'red'"
  [input]
  (let [lr (.lastIndexOf input ":\"red\"")]
    (when (>= lr 0)
      (let [llrs (subs input 0 lr)
            rlrs (subs input lr)
            ob (where-jsobj-starts llrs)
            cb (where-jsobj-ends rlrs)]
        (str (subs input 0 (dec ob)) (subs rlrs (inc cb)))))))

(defn day12 [input]
  (reduce #(if %2
            %2
            (reduced (reduce
                       +
                       (map
                         (fn [n] (s->int n)) (re-seq #"\-?\d+" %))))) (iterate rm-red-jsobj input)))

;day 13
;parts 1 and 2 (part 2 is about changing input)
(defn- d13-single-line-extractor
  "Extracts data from single line of input for day 13"
  [line]
  (let [[who1 _ what how-much _ _ _ _ _ _ who2] (re-seq #"\w+\b" line)
        amount (s->int how-much)
        delta (if (= what "gain") amount (- amount))]
    [[who1 who2] delta]))

(defn- d13-persons
  "Collects all persons present around the round table :)"
  [deltas]
  (reduce #(let [[endpoints _] %2]
            (conj % (first endpoints) (last endpoints))) #{} deltas))

(defn day13
  [input]
  "Solves day 13 part 1"
  (let [deltas (into {} (map d13-single-line-extractor input))
        persons (d13-persons deltas)
        arrangements (permutations persons)
        happiness (fn [members]
                    (let [pairs (map vec (partition 2 1 members))]
                      (reduce
                        #(+ % (deltas %2))
                        0
                        (conj pairs [(ffirst pairs) (last (last pairs))]))))]
    (apply max
           (map
             #(+ (happiness %) (happiness (reverse %)))
             arrangements))))

;day 14
;part 1
(defn- d14-single-line-extractor
  [line]
  (let [[n s d r] (map first (re-seq #"(^\w+)|\d+" line))]
    {:name               n
     :moment-in-time     1
     :speed              (s->int s)
     :duration           (s->int d)
     :rest               (s->int r)
     :current-state      :idle
     :current-state-time 0
     :distance-travelled 0}))

(defn- move-raindeer
  [rd]
  (update rd :distance-travelled #(+ % (:speed rd))))

(defn- go-rest
  [rd]
  (-> rd
      (assoc :current-state :resting)
      (assoc :current-state-time 1)))

(defn- go-fly
  [rd]
  (-> rd
      (move-raindeer)
      (assoc :current-state :flying)
      (assoc :current-state-time 1)))

(defn- simply-fly
  [rd]
  (-> rd
      (move-raindeer)
      (update :current-state-time inc)))

(defn- simply-rest
  [rd]
  (update rd :current-state-time inc))

(defn- transit-rd
  [rd ref-state f1 f2]
  (if (< (:current-state-time rd) (get rd ref-state))
    (f1 rd)
    (f2 rd)))

(defn- go-raindeer
  "Raindeer rd calculation"
  [rd]
  (let [cs (:current-state rd)
        transitioned (case cs
                       :idle (go-fly rd)
                       :flying (transit-rd rd :duration simply-fly go-rest)
                       :resting (transit-rd rd :rest simply-rest go-fly))]
    (update transitioned :moment-in-time inc)))

(defn- d14-part2-reporter
  [reporter value]
  (let [key (:moment-in-time value)]
    (update reporter key #(conj % %2) [(:name value) (:distance-travelled value)])))

(defn- find-max-points [data]
  (apply max
         (map second
              (reduce
                (fn [a [_ i]]
                  (let [m (apply max (map second i))
                        who-has-max (filter #(= m (second %)) i)]
                    (reduce #(update % (first %2) (fn [v] (if (nil? v) 1 (inc v)))) a who-has-max)))
                {}
                data))))

(defn- find-longest-distance-travelled [raindeers]
  (:distance-travelled (reduce
                         #(if (> (:distance-travelled %2) (:distance-travelled %)) %2 %)
                         (first raindeers)
                         (rest raindeers))))

(defn day14
  "Expects sequence of lines from file - easily achieved by using day-input-line-seq like (day-input-line-seq 14)"
  [input]
  (let [collector (agent (into {} (map #(identity [% []]) (range 2 2504))))
        watcher (fn [_ _ _ new]
                  (send collector d14-part2-reporter new))
        raindeers (map #(let [a (agent (d14-single-line-extractor %))]
                         (add-watch a :reporter watcher)
                         a)
                       input)]
    (doall
      (repeatedly 2503 #(doall
                         (for [r raindeers] (send r go-raindeer)))))
    (apply await raindeers)
    (await collector)
    (let [values (map #(identity @%) raindeers)]
      {:part1 (find-longest-distance-travelled values)
       :part2 (find-max-points @collector)})))
