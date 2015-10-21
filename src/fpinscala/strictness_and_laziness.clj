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

;auxiliary - direct translation from book - WARNING! IT IS NOT REALLY LAZY as I am currently not sure how to mimic named/lazy params
(defn fold-right [z f s]
  (let [[h & t] s]
    (if h
      (f h (fold-right z f t))
      z)))
;it would be more concise should I have used core.match

;example - exists via f-r - also direct translation from book
(defn exists
  "Function exists via fold-right from fpins"
  [p s]
  (fold-right false #(or %2 (p %)) s))

;5.4
;mine
(defn for-all
  "forAll for 5.4 in FPINS, it is lazy but much more verbose than fpins solution and I should have noticed f-r should be used!"
  [p s]
  (let [[h & t] s]
    (if t
      (and (p h) (for-all p t))
      (p h))))
;fpins
(defn for-all-fpins
  "forAll as in fpins, I should have observed that f-r should be used! But mine works as well and it is lazy also"
  [p s]
  (fold-right true #(and %2 (p %)) s))
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

;5.6
;mine
(defn head-option-fr
  "headOption from fpins via f-r"
  [s]
  (fold-right {:val nil :none true} (fn [v _] (if v {:val v :none false})) s))
;fpins
;conceptually the same - woo-hoo :)
;-----------------------------------------------------------------------

;5.7
;mine
(defn map-fr
  "map via f-r"
  [f s]
  (fold-right '() #(conj %2 (f %)) s))
(defn filter-fr
  "filter via f-r"
  [p s]
  (fold-right '() #(if (p %) (conj %2 %) %2) s))
(defn append-fr
  "append via f-r"
  [sl sr]
  (fold-right sr #(conj %2 %) sl))
(defn flat-map-fr
  "flatMap via f-r"
  [f s]
  (fold-right '() #(append-fr (f %) %2) s))
;fpins
;all conceptually the same with exception that I simply translated flat-map by accident :)
;furthermore I have had incorrect order due to argument swapping
;-----------------------------------------------------------------------

;5.8
;mine
(defn constant
  "Returns inifinite stream of given value"
  [x]
  (stream-cons x (lazy-seq (constant x))))
;fpins
;I am not really sure if this is good variant and if it
;really reassembles fpins solution, anyway it is done through Clojure's core functions.
;And I am not really sure how to reassemble fpins as Clojure doesn't provide lazy vals/args
(defn constant-fpins
  [x]
  (repeat x))
;-----------------------------------------------------------------------

;5.9
;mine
(defn from
  "Returns range of integers starting from n"
  [n]
  (stream-cons n (lazy-seq (from (inc n)))))
;fpins
;conceptually - the same

;5.10
;mine
(defn fibs
  "Fibonacci sequence"
  []
  (letfn [(go [x0, x1]
              (cons x0 (lazy-seq (go x1, (+ x0 x1)))))]
    (stream-cons 0 (go 1 1))))
;fpins
(defn fibs-fpins                                            ; well :) I should have seen it... there is unnecessary stream-cons :)
  "Fibonacci sequence"
  []
  (letfn [(go [x0, x1]
              (cons x0 (lazy-seq (go x1, (+ x0 x1)))))]
    (go 0 1)))
;-----------------------------------------------------------------------

;5.11
;mine
(defn unfold
  "Unfold for exercise 5.11"
  [z f]                                                     ;f produces both next value and next state
  (when-let [val (f z)]
    (cons val (lazy-seq (unfold val f)))))
;fpins
(defn unfold-fpins
  "Unfold for exercise 5.11"
  [z f]                                                     ;f produces both next value and next state
  (when-let [[h s] (f z)]
    (cons h (lazy-seq (unfold-fpins s f)))))                      ;here is difference between mine and fpins :)
;obviously I haven't fully comprehend point of unfold
;and to use elements of "tuple" (vector in Clojure's case) appropriately
;-----------------------------------------------------------------------

;5.12 - fibs
;mine
(defn fibs-via-unfold
  "Fibonacci sequence via unfold"
  []
  (cons 0 (map first (unfold [0 1] #(let [x0 (first %) x1 (second %)]
                                     [x1 (+ x0 x1)])))))
;fpins
(defn fibs-via-unfold-fpins
  "Fibonacci sequence via unfold by fpins"
  []
  (unfold-fpins [0 1] #(when-let [[f0 f1] %]
                        [f0 [f1 (+ f0 f1)]])))
;-----------------------------------------------------------------------

;5.12 - from
;mine - I will use unfold-fpins as it is obviously better implementation
(defn from-via-unfold
  "From sequence via unfold"
  [x]
  (unfold-fpins x #(do [% (inc %)])))
;fpins
;well, conceptually the same, although I was wandering somewhat
;with starting with vector [x (inc x)] what worked but with this
;implementation destructuring (or "pattern matching" if you like) is not necessary

;5.12 - constant
;mine
(defn constant-via-unfold
  "Constant via unfold"
  [x]
  (unfold-fpins x #(do [% %])))
;fpins
;conceptually - the same

;5.12 - ones
;mine
(defn ones-via-unfold
  "Ones via unfold"
  []
  (unfold-fpins 1 #(do [% %])))
;fpins
;conceptually - the same
;mine via mine unfold :) - THIS IS EXAMPLE HOW NOT TO DO IT :)
(defn ones-via-mine-unfold
  "Ones via my wrong implementation of unfold"
  []
  (map first (unfold [1 1] #(when-let [[x0 x1] %]
                             [x0 x1]))))

;5.13 - map
;mine
(defn map-via-unfold [f s]
  (unfold-fpins s #(let [[h & t] %]
                    (when h
                      [(f h) t]))))
;fpins - more or less the same
;as fpins variant is using pattern match with function which (raw) Clojure lacks

;5.13 - take
;mine
(defn take-via-unfold
  "Take via unfold for exercise 5.13 of fpins"
  [n s]
  (unfold-fpins [s 1] #(let [[[h & t] cnt] %]
                        (when (and h (<= cnt n))
                          [h [t (inc cnt)]]))))
;fpins
;conceptually - the same, just counting backwards and pattern matching on 1
;which I am awoiding as I am sticking to raw clojure without involving core.match

;5.13 - takeWhile
;mine
(defn take-while-via-unfold
  "Take-while via unfold for exercise 5.13 of fpins"
  [f s]
  (unfold-fpins s #(let [[h & t] %]
                    (when (and h (f h))
                      [h t]))))
;fpins
;conceptually - the same