(ns fpinscala.strictness-and-laziness)

(defn stream-cons
  "Should reassemble Stream trait from fpins"
  [h t]
  (lazy-seq
    (cons h (lazy-seq t))))
;I am using lazy-seq here as there is no "lazy syntax" in Clojure for
;lazy evaluation as there is in Scala

;Clojure is dynamic and treats nil as false,
; so "Option" is not truly required
;headOption f brings down to
(defn head-option
  "Simple reassemblance of headOption function from fpins"
  [s]
  (first s))

;5.1
;mine
;although I could use (list) function of Clojure, let's try manual work...
(defn to-list [s]
  (loop [loc-s s acc []]                                    ;intentionally using vector then conversion to list to avoid reversing list...
    (if (seq loc-s)
      (recur (rest loc-s) (conj acc (first loc-s)))
      (list* acc))))
;fpins
;well, no point in translation as solution is similar to mine above
;just to point out in fpins, list is used which is reversed...
;as I pointed out in comment above... fpins is using mutable buffer to
;remove reversing
;-----------------------------------------------------------------------

;5.2
;mine
;take
(defn my-take [n s]
  (letfn [(go [x s acc]
              (if (= x n)
                acc
                (recur (inc x) (rest s) (conj acc (first s)))))]
    (go 0 s [])))
;drop
(defn my-drop [n s]
  (letfn [(go [x s acc]
              (if (< x n)
                (recur (inc x) (rest s) acc)
                (recur x (rest s) (conj acc (first s)))))]
    (go 0 s [])))
;my-drop is not lazy though...it operates properly with "my-take" but
;evaluates to the bottom otherwise... that could be improved.
;fpins
;take - conceptually similar
;drop - better than mine, preserves lazyness
(defn my-drop-fpins [x s]
  (if (seq s)
    (if (> x 0)
      (recur (dec x) (rest s))
      s)
    s))
;-----------------------------------------------------------------------

;5.3
;mine
(defn my-take-while [f s]
  (letfn [(go [s acc]
              (if (seq s)
                (let [value (first s)]
                  (if (f value)
                    (recur (rest s) (conj acc value))
                    acc))
                acc))]
    (go s [])))
;fpins
(defn my-take-while-fpins
  "fpins variant of take-while"                             ;obviously - less verbose than mine and no (obvious) accumulation :)
  [f s]
  (let [value (first s)]
    (when (f value)
      (stream-cons value (my-take-while-fpins f (rest s))))))
;-----------------------------------------------------------------------

;auxiliary - direct translation from book
(defn fold-right [z f s]
  (let [[h & t] s]
    (if t
      (f h (fold-right z f t))
      (if h (f h z) z))))
;it would be more concise should I have used core.match

;example - exists via f-r - also direct translation from book
(defn exists
  "Function exists via fold-right from fpins"
  [p s]
  (fold-right false #(or (p %) %2) s))

;5.4
;mine
(defn for-all
  "forAll for 5.4 in FPINS, it is lazy but much more verbose than fpins solution and I shoul have noticed f-r should be used!"
  [p s]
  (let [[h & t] s]
    (if t
      (and (p h) (for-all p t))
      (p h))))
;fpins
(defn for-all-fpins
  "forAll as in fpins, I should have observed that f-r should be used! But mine works as well and it is lazy also"
  [p s]
  (fold-right true #(and (p %) %2) s))
;-----------------------------------------------------------------------

;5.5
;mine
(defn take-while-fr
  "Reimplementing take-while via f-r"
  [p s]
  (fold-right '() #(if (p %) (conj %2 %) %2) s))
;fpins
;conceptually the same
;-----------------------------------------------------------------------
