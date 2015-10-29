(ns fpinscala.purely-functional-state
  (:use [fpinscala.strictness-and-laziness :only [unfold-fpins take-via-unfold fold-right]]))

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
(defn map-rng-2
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
  (map-rng-2 ra rb #(identity [% %2])))

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
    #(map-rng-2 % %2 (fn [a b]
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
    #(map-rng-2 % %2 (fn [a b] (cons a b)))                 ;correct to say it is "initial" but rather neutral (or terminating?) element
    fs))
;THIS WORKS!!! - THUS IN TERMS OF FPINS (AS FAR AS I UNDERSTOOD) Z IS UNIT/NEUTRAL ELEMENT

(defn random-ints-via-rng-sequence [n rng]
  ((rng-sequence-fpins (repeat n non-negative-int)) rng))
