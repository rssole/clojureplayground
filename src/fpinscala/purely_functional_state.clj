(ns fpinscala.purely-functional-state
  (:use [fpinscala.strictness-and-laziness :only [unfold-fpins take-via-unfold fold-right]])
  (:require [clojure.core.match :refer [match]]))

(declare simple-rng)

(defn make-simple-rng
  "Constructor fn for simple RNGs"
  [seed]
  (fn []
    (simple-rng seed)))

(defn- make-result
  "Auxiliary function to avoid same map creation all over the place"
  [n nxt-rng]
  {:n n :nxt-rng nxt-rng})

(defn- simple-rng
  "As per listing 6.2 from FPINS"
  [^long seed]
  ;Type hint needed here so multiplication below does not yield int but rather long
  (let [nseed (bit-and (+ (unchecked-multiply seed 0x5deece66d) 0xb) 0xffffffffffff)
        ;unchecked-multiply is falling back to Java semantic of multiplication
        ;to make this working same way as in Scala/FPINS option
        nxt-rng (make-simple-rng nseed)
        n (.intValue (unsigned-bit-shift-right nseed 16))]
    (make-result n nxt-rng)))

(defmacro from-rng [rng _ bindings & body]
  `(let [{~@(interleave bindings [:n :nxt-rng]) ~@[]} ~rng]
     ~@body))

;6.1
;mine
(defn non-negative-int
  "Generates random integer between 0 and Int.maxValue (inclusive)"
  [rng]
  (from-rng (rng) :let [n nxt-rng]
            (if (> n Integer/MIN_VALUE)
              (make-result (Math/abs ^int n) nxt-rng)
              (make-result Integer/MAX_VALUE nxt-rng))))
;fpins
(defn non-negative-int-fpins
  "FPINS variant of above function - haha - and I was thinking: Adding one is way too simple :)"
  [rng]
  (from-rng (rng) :let [n nxt-rng]
            (if (< n 0)
              (make-result (- (inc n)) nxt-rng)
              (make-result n nxt-rng))))
;-----------------------------------------------------------------------

;6.2
;mine
(defn a-double
  "Generates Double between 0 (inclusive) and 1 (exclusive)"
  [rng]
  (from-rng (non-negative-int rng) :let [n nxt-rng]
            (make-result (double (/ n Integer/MAX_VALUE)) nxt-rng)))
;fpins
(defn a-double-fpins
  "Generates Double between 0 (inclusive) and 1 (exclusive)"
  [rng]
  (from-rng (non-negative-int rng) :let [n nxt-rng]
            (make-result
              (/ n (inc (.doubleValue Integer/MAX_VALUE)))
              nxt-rng)))
;-----------------------------------------------------------------------

;6.3
;mine
(defn int-double
  "Generates (int, double) pair"
  [rng]
  (from-rng (non-negative-int rng) :let [n nxt-rng]
            (from-rng (a-double nxt-rng) :let [dbl n-rng]
                      (make-result [n dbl] n-rng))))

(defn double-int
  "Generates (double, int) pair"
  [rng]
  (from-rng (a-double rng) :let [dbl nxt-rng]
            (from-rng (non-negative-int nxt-rng) :let [n n-rng]
                      (make-result [dbl n] n-rng))))

(defn triple-double
  "Generates 3-tuple (double, double, double)"
  [rng]
  (from-rng (a-double rng) :let [d1 r1]
            (from-rng (a-double r1) :let [d2 r2]
                      (from-rng (a-double r2) :let [d3 r3]
                                (make-result [d1 d2 d3] r3)))))
;fpins
;well conceptually all the same stuff with difference I've introduced
;macro (for fun and exercise :) )
;-----------------------------------------------------------------------

;6.4
;mine
(defn random-ints
  "Generates list of random integers"
  [n rng]
  (take-via-unfold n (unfold-fpins
                       (rng)
                       (fn [nstate]
                         (from-rng nstate :let [n nxt-rng]
                                   [n (nxt-rng)])))))
;fpins
;I've used unfold, whereas fpins is using plain recursion
;and my solution does not return last rng so my solution is only partial :|
(defn random-ints-fpins-1
  "Generates list of random integers - fpins variant 1"
  [n rng]
  (if (zero? n)
    ['() rng]
    (from-rng (rng) :let [x r1]
              (let [[xs, r2] (random-ints-fpins-1 (dec n) r1)]
                [(cons x xs) r2]))))

(defn random-ints-fpins-2
  "Generates list of random integers - fpins variant 2 - TAIL RECURSIVE - outcome is reversed though but apparently it does not matter"
  [n rng]
  (letfn [(go [cnt r xs]
            (if (zero? cnt)
              [xs r]
              (from-rng (r) :let [x r2]
                        (go (dec cnt) r2 (cons x xs)))))]
    (go n rng '())))
;-----------------------------------------------------------------------

(defn unit
  "Unit function from section 6.4 of fpins"
  [a]
  (fn [rng] (make-result a rng)))

(defn map-rng
  "Mapping combinator from section 6.4 of fpins"
  [s f]
  (fn [rng]
    (from-rng (s rng) :let [n n-rng]
              (make-result (f n) n-rng))))

(def non-negative-even-int
  "Generates non-negative even int and is constructed by means of combinator"
  (map-rng non-negative-int-fpins #(- % (mod % 2))))

;6.5
;mine
(def a-double-via-map-comb
  "Reimplementing double via mapping combinator"
  (map-rng non-negative-int #(/ % (inc (.doubleValue Integer/MAX_VALUE)))))
;fpins
;Conceptually - the same
;-----------------------------------------------------------------------

;6.6
;mine
(defn map2-rng
  "map2 for exercise 6.6, takes two actions ra and rb and binary function f which combines results of them"
  [ra rb f]
  (fn [rng]
    (from-rng (ra rng) :let [n1 r1]
              (from-rng (rb r1) :let [n2 r2]
                        (make-result (f n1 n2) r2)))))
;fpins
;Conceptually - the same

(defn both
  "Translating 'both' from section 6.4.1 - Combining state actions"
  [ra rb]
  (map2-rng ra rb #(identity [% %2])))

(def int-double-via-both
  "int-double via both (via map2 (: )"
  (both non-negative-int-fpins a-double-fpins))

(def double-int-via-both
  "double-int via both (via map2 (: )"
  (both a-double-fpins non-negative-int))
;-----------------------------------------------------------------------

;6.7
;mine
(defn rng-sequence
  "sequence function for exercise 6.7"
  [fs]
  (reduce                                                   ;check out comment below to see why I've used reduce instead of fold-right
    #(map2-rng % %2 (fn [a b]
                      (if (vector? a)
                        (conj a b)
                        [a b])))
    (first fs)
    (rest fs)))
;I am using reduce because my fold-right from strictness-and-laziness is not proper scala translation
;and does not work correctly...
;UPDATE [29.10.2015.] Errrr about above, well not necessarily if z parameter is considered
;as "unit" or "neutral" folding element. See comments around fpinscala.strictness-and-laziness/fold-right
;UPDATE [29.10.2015.] As per fpins variant (see below) fold-right is quite ok in this case as well
;fpins
(defn rng-sequence-fpins
  "sequence function for exercise 6.7 FPINS variant"
  [fs]
  (fpinscala.strictness-and-laziness/fold-right             ;Notice that this variant is using fold-right
    (unit '())                                              ;Check out that "unit-produced" function is used as z value (now - it wouldn't be really
    #(map2-rng % %2 (fn [a b] (cons a b)))                  ;correct to say it is "initial" but rather neutral (or terminating?) element
    fs))
;THIS WORKS!!! - THUS IN TERMS OF FPINS (AS FAR AS I UNDERSTOOD) Z IS UNIT/NEUTRAL ELEMENT

(defn random-ints-via-rng-sequence [n rng]
  ((rng-sequence-fpins (repeat n non-negative-int)) rng))

;6.8
;mine
;I haven't really got the point of what flat-map should look like (I did actually got to some point but got stuck there)
;I missed the point that g is returning Rand[B] function (check exercise 6.8 text, page 87) and that it
;accepts RNG
;fpins
(defn flat-map
  "flatMap for exercise 6.8 from FPINS"
  [f g]
  #(from-rng (f %) :let [n rng2]
             ((g n) rng2)))

;nonNegativeLessThan via flatMap
(defn non-negative-less-than
  "nonNegativeLessThan for exercise 6.8 of FPINS"
  [n]
  (flat-map non-negative-int (fn [x]
                               (let [m (mod x n)]
                                 (if (>= (- (+ x (dec n)) m) 0)
                                   ; Ha! I got it right that >>unit<< should be used here
                                   (unit m)
                                   (non-negative-less-than n))))))
;fpins
;Conceptually - the same

;6.9
;mine
(defn map-via-flat-map
  "map in terms of flatMap for exercise 6.9 of FPINS"
  [s f]
  (flat-map s (fn [x] (unit (f x)))))

;usage example
(def non-negative-int-via-map-via-flat-map
  (map-via-flat-map non-negative-int-fpins #(- % (mod % 2))))

(defn map2-via-flat-map
  "map2 in terms of flatMap for exercise 6.9 of FPINS"
  [ra rb f]
  (flat-map ra (fn [n1]
                 (fn [r1]
                   (from-rng (rb r1) :let [n2 r2]
                             (make-result (f n1 n2) r2))))))

;usage example
(defn both-via-map2-via-flat-map
  "Translating 'both' from section 6.4.1 - Combining state actions"
  [ra rb]
  (map2-via-flat-map ra rb #(identity [% %2])))

(def int-double-via-both-map2-flat-map
  "int-double via both (via map2 (: )"
  (both-via-map2-via-flat-map non-negative-int-fpins a-double-fpins))

(def double-int-via-both-map2-flat-map
  "double-int via both (via map2 (: )"
  (both-via-map2-via-flat-map a-double-fpins non-negative-int))

;fpins
;Check this out, this is much better :) of course :)
(defn map2-via-flat-map-fpins
  [ra rb f]
  (flat-map ra (fn [a] (map-rng rb #(f a %)))))

(defn both-via-map2-via-flat-map-fpins
  "Translating 'both' from section 6.4.1 - Combining state actions"
  [ra rb]
  (map2-via-flat-map-fpins ra rb #(identity [% %2])))

(def int-double-via-both-map2-flat-map-fpins
  "int-double via both (via map2 (: )"
  (both-via-map2-via-flat-map non-negative-int-fpins a-double-fpins))

(def double-int-via-both-map2-flat-map-fpins
  "double-int via both (via map2 (: )"
  (both-via-map2-via-flat-map-fpins a-double-fpins non-negative-int))

;6.10
;Well, given Clojure's dynamic nature, I have nothing special to do
;for State type and functions apart from re-defining supporting from-rng macro
;and make-result function to use more generic names for value and new state map entries
(defmacro from-state [state _ bindings & body]
  `(let [{~@(interleave bindings [:val :next-state]) ~@[]} ~state]
     ~@body))

(defn- make-state
  "Auxiliary function to avoid same map creation all over the place"
  [val next-state]
  {:val val :next-state next-state})
;In addition, Clojure does not provide case classes and companion objects as Scala
;does so existing unit, map, map2, flat-map and sequnce does the job as
;there is no instance you could invoke method on

(defn flat-map-s
  "flatMap for exercise 6.8 from FPINS but generalized to state for exercise 6.10"
  [f g]
  #(from-state (f %) :let [v s]
               ((g v) s)))

(defn unit-state
  "Generalized Unit function from section 6.5 of fpins"
  [a]
  (fn [rng] (make-state a rng)))

(defn map-via-flat-map-s
  "Generalized version of map-via-flat-map from exercise 6.9, for exercise 6.10"
  [s f]
  (flat-map-s s (fn [x] (unit-state (f x)))))

(defn map2-via-flat-map-s
  [s1 s2 f]
  (flat-map-s s1 (fn [a] (map-via-flat-map-s s2 #(f a %)))))

(defn state-sequence-fpins
  "sequence function for exercise 6.7 FPINS variant"
  [fs]
  (fpinscala.strictness-and-laziness/fold-right             ;Notice that this variant is using fold-right
    (unit-state '())                                        ;Check out that "unit-produced" function is used as z value (now - it wouldn't be really
    #(map2-via-flat-map-s % %2 (fn [a b] (cons a b)))       ;correct to say it is "initial" but rather neutral (or terminating?) element
    fs))                                                    ;UPDATE (25.11.2015) Check out wikipedia about monad, it is actually "return" also
;called "unit" operation of a monad

(declare simple-rng-s)

(defn make-simple-rng-s
  "Constructor fn for simple RNGs but customized to work with general state"
  [seed]
  (fn []
    (simple-rng-s seed)))

(defn- simple-rng-s
  "As per listing 6.2 from FPINS"
  [^long seed]
  ;Type hint needed here so multiplication below does not yield int but rather long
  (let [nseed (bit-and (+ (unchecked-multiply seed 0x5deece66d) 0xb) 0xffffffffffff)
        ;unchecked-multiply is falling back to Java semantic of multiplication
        ;to make this working same way as in Scala/FPINS option
        nxt-rng (make-simple-rng-s nseed)
        n (.intValue (unsigned-bit-shift-right nseed 16))]
    (make-state n nxt-rng)))

(defn non-negative-int-fpins-s
  "FPINS variant of non-negative-int function, adapted to 'state' variant - haha - and I was thinking: Adding one is way too simple :)"
  [rng]
  (from-state (rng) :let [n nxt-rng]
              (if (< n 0)
                (make-state (- (inc n)) nxt-rng)
                (make-state n nxt-rng))))

;usage example
((state-sequence-fpins (repeat 5 non-negative-int-fpins-s)) (make-simple-rng-s 42))

;map and flat-map difference (afaic):
;map accepts two functions where first function
;produces state and second function simply
;accepts value, maps it and returns new state along with value
;flat-map also accepts two function where, again, first one
;accepts produces some state however, second function accepts value
;but returns function which again accepts some state and will
;produce new value and next state. This yields flexibility which
;plain map lacks, flat-map is able to adjust behavior
;based on value

;here - I try to mimic get, set and modify of FPINS/6.6
(def get-state (fn [s] (make-state (:val s) (:val s))))
(def set-state (fn [s] (make-state nil (:val s))))
(def modify-state (fn [f]
                    (fn [s]
                      (let [ns (get-state s)]
                        (set-state (f ns))))))
;now when step-machine is in place - to re-think get, set and modify...

;here is step-machine function from Runar Bjarnasson's explanation of 6.1 on fpins google group
;it is using core.match as writing with if, cond etc would make it ridiculously complex

(defn machine [locked candies coins]
  {:locked locked :candies candies :coins coins})

;Hey Rastko, yes you! You need to read more carefully :)
;well, it seems I'd need to know Scala a bit better.
;In fpins solution Candy.update is function that produces function
;to be more specific, for given input it produces function which accepts state (machine)
;and produces function which matches given input and provided state
(defn step-machine
  "inputs is sequence of either coins or turns, s is 'machine' in focus"
  [i]
  (fn [s]
    (match [i s]
         [_ {:locked _ :candies 0 :coins _}] s
         [:coin {:locked false :candies _ :coins _}] s
         [:turn {:locked true :candies _ :coins _}] s
         [:coin {:locked true :candies candies :coins coins}]
         (machine false candies (inc coins))
         [:turn {:locked false :candies candies :coins coins}]
         (machine true (dec candies) coins))))



