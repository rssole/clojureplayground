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
;fpins
;-----------------------------------------------------------------------