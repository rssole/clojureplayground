(ns fpinscala.purely-functional-state)

(declare simple-rng)

(defn make-simple-rng
  "Constructor fn for simple RNGs"
  [seed]
  (fn []
    (simple-rng seed)))

(defn- simple-rng
  "As per listing 6.2 from FPINS"
  [^long seed]
  (let [nseed (bit-and (+ (unchecked-multiply seed 0x5deece66d) 0xb) 0xffffffffffff)
        ;unchecked-multiply is falling back to Java semantic of multiplication
        ;to make this working same way as in Scala/FPINS option
        nxt-rng (make-simple-rng nseed)
        n (.intValue (unsigned-bit-shift-right nseed 16))]
    {:n n :nxt-rng nxt-rng}))


;6.1
