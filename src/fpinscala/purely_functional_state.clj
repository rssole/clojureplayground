(ns fpinscala.purely-functional-state)

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
