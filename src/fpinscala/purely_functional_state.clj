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
  `(let [{:keys ~bindings} ~rng]
     ~@body))

;6.1
;mine
(defn non-negative-int
  "Generates random integer between 0 and Int.maxValue (inclusive)"
  [rng]
  (from-rng (rng) :use [n nxt-rng]
    (if (> n Integer/MIN_VALUE)
      (make-result (Math/abs ^int n) nxt-rng)
      (make-result Integer/MAX_VALUE nxt-rng))))
;fpins
(defn non-negative-int-fpins
  "FPINS variant of above function - haha - and I was thinking: Adding one is way too simple :)"
  [rng]
  (from-rng (rng) :use [n nxt-rng]
    (if (< n 0)
      (make-result (- (inc n)) nxt-rng)
      (make-result n nxt-rng))))
;-----------------------------------------------------------------------

;6.2
;mine
(defn a-double
  "Generates Double between 0 (inclusive) and 1 (exclusive)"
  [rng]
  (from-rng (non-negative-int rng) :use [n nxt-rng]
            (make-result (double (/ n Integer/MAX_VALUE)) nxt-rng)))
;fpins
(defn a-double-fpins
  "Generates Double between 0 (inclusive) and 1 (exclusive)"
  [rng]
  (from-rng (non-negative-int rng) :use [n nxt-rng]
            (make-result
              (/ n (inc (.doubleValue Integer/MAX_VALUE)))
              nxt-rng)))
;-----------------------------------------------------------------------
