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

;6.1
;mine
(defn non-negative-int
  "Generates random integer between 0 and Int.maxValue (inclusive)"
  [rng]
  (let [{next-int :n nxt-rng :nxt-rng} (rng)]
    (if (> next-int Integer/MIN_VALUE)
      (make-result (Math/abs ^int next-int) nxt-rng)
      (make-result Integer/MAX_VALUE nxt-rng))))
