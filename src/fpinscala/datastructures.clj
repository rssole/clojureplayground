(ns fpinscala.datastructures
  (:require [clojure.test :refer [with-test is]]
            [clojure.core.match :refer [match]]))

(defn append [xs ys]
  (if (nil? xs)
    ys
    (cons (first xs) (append (next xs) ys))))

;exercise 3.2
;mine
(defn tail-option-1 [xs]
  (if (not xs)
    (list)
    (drop 1 xs)))
(defn tail-option-2 [xs]
  (if-let [[_ & t] xs]                                      ;Note that [[& tail] xs] yielded nothing thus - BUG
    t
    (list)))
;fpinscala
;simply uses pattern matching
;similar to option 2 above
;-----------------------------------------------------------------------

;exercise 3.3
;mine
(defn set-head [x xs]
  (let [[& t] xs]
    (cons x t)))
;fpinscala
;simply uses pattern matching
;similar to my destructuring above
;-----------------------------------------------------------------------

;exercise 3.4
;mine
(defn my-drop [n xs]
  (loop [cnt n l xs]
    (if (= cnt 0)
      l
      (recur (dec cnt) (tail-option-2 l)))))                ;There was bug in tail-option-2 (see above) which has been fixed
;fpinscala
;uses pattern matching but same/similar idea :)
;-----------------------------------------------------------------------

;exercise 3.5
;mine
(defn my-drop-while [f xs]
  (loop [l xs]
    (if (or (empty? l) (not (f (first l))))
      l
      (recur (tail-option-2 l)))))
;fpinscala
;similarly uses tail but not function but rather,
;again, scala pattern matching with "pattern guard"
;-----------------------------------------------------------------------

;exercise 3.6
;mine
(defn my-init [xs]
  (loop [l xs acc ()]
    (let [[h & t] l]
      (if (not t)
        (reverse acc)
        (recur t (cons h acc))))))
;fpinscala
(defn init-fpins [xs]
  (loop [[h & t] xs]
    (if (not t)
      nil
      (cons h (init-fpins t)))))
;note that in answers - it is mentioned that there is another common
;convention to do reversing - just like I did :)
;-----------------------------------------------------------------------

; fold-right as it is in fpins book (page 39)
;it could be altered to use clojure's loop-recur but let it be like this
(defn fold-right [as z f]
  (let [[x & xs :as orig] as]
    (if (nil? orig)
      z
      (f x (fold-right xs z f)))))

;couple of more from fpins (page 39)
(defn sum2 [xs]
  (fold-right xs 0 #(+ % %2)))

(defn product2 [xs]
  (fold-right xs 1 #(* % %2)))

;3.7 - I think not because of stack... how to skip over stack entries before
;we spot 0.0?
;-----------------------------------------------------------------------

;3.8 - it yields equivalent list as input
;check out for thorough explanation in exercises answers of fpins
;-----------------------------------------------------------------------

;3.9
;mine
(defn length [xs]
  (fold-right xs 0 (fn [_ x] (+ x 1))))
;fpins
;conceptually - same :)
;-----------------------------------------------------------------------

;3.10
;mine
(defn fold-left [xs z f]
  (loop [[x & others :as original] xs acc z]
    (if (nil? original)
      acc
      (recur others (f acc x)))))                           ;here it was (f x acc) thus
;my fold-r-via-fold-l-1 didn't work :)
;after swapping to (f acc x) it worked :)
;
;fpins
;relies on @tailrec scala annotation
;and explicit recursive function invocation
;but basically mine is equivalent solution as
;clojure is using loop->recur for tail recursive calls
;@annotation.tailrec
;def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
; case Nil => z
; case Cons (h, t) => foldLeft (t, f (z, h)) (f)
; }
; Actually - nope mine is not really equiv because below fold-r-via-fold-l-1 fails
;this should be better:
(defn fold-left-fpins [l z f]
  (let [[h & t :as original] l]
    (if (nil? original)
      z
      (fold-left-fpins t (f z h) f))))
;-----------------------------------------------------------------------

;3.11
;mine
(defn sum-fl [xs]
  (fold-left xs 0 +))
(defn product-fl [xs]
  (fold-left xs 1 *))
(defn len-fl [xs]
  (fold-left xs 0 (fn [_ x] (+ 1 x))))
;fpins
;All are basically same :)
;-----------------------------------------------------------------------

;3.12
;mine
(defn my-reverse [xs]
  (fold-left xs '() #(cons %2 %)))
(defn my-reverse2 [xs]
  (fold-right xs [] #(conj %2 %)))                          ;This is
;way to do it with fold-right as list does not support adding at the end
;fpins
;It is done via f-l as in my first attempt
;-----------------------------------------------------------------------

;3.13
;mine (I have cheated here and looked into answer but just for accumulator function :) )
(defn fold-r-via-fold-l [xs z f]
  (fold-left (my-reverse xs) z (fn [b a] (f a b))))
(defn fold-r-via-fold-l-1 [xs z f]
  ((fold-left xs (fn [b] b) (fn [g a]
                              (fn [b]
                                (g (f a b))))) z))
;Check answer-rewrite below as I simply used answer :|
;fpins
;this is by the book literal translation as fold-left-fpins is literal translation as well (see above)
(defn fold-r-via-fold-l-1-fpins [xs z f]
  ((fold-left-fpins xs (fn [b] b) (fn [g a]
                                    (fn [b]
                                      (g (f a b))))) z))
(defn fold-l-via-fold-r [xs z f]
  ((fold-right xs (fn [b] b) (fn [a g]
                               (fn [b]
                                 (g (f b a))))) z))
;-----------------------------------------------------------------------

;3.14
;mine
(defn append-via-fold-right [xs ys]
  (fold-right xs ys cons))
;fpins
;basically - the same :)
;-----------------------------------------------------------------------

;3.15
;mine
;(defn list-concat [ls]
;  (let [[h & t] ls]
;    (fold-left t h #(append %2 %)))) ; before fixing append (swapped args xs ys)
; there was ys xs before
(defn list-concat [ls]
  (let [[h & t] ls]
    (fold-left t h append-via-fold-right)))
;fpins
;they did it via fold right what is even better (more concise that is)
(defn list-concat-fpins [ls]
  (fold-right ls nil append-via-fold-right))
;-----------------------------------------------------------------------

;3.16
;mine
(defn add-one [xs]
  (fold-right xs '() #(cons (inc %) %2)))
;fpins
;basically - the same :) I have tried with f-l but very wisely I forgot it
;reverses list so not what is required
;-----------------------------------------------------------------------

;3.17
;mine
(defn to-str [xs]
  (fold-right xs '() #(cons (str %) %2)))
;fpins
;basically - the same :)
;-----------------------------------------------------------------------

;3.18
;mine
(defn my-map [xs f]
  (fold-right xs '() #(cons (f %) %2)))
;fpins
;multiple variations
;0) using fold-right (but with note it is not stack safe) - same as mine
;1) using fold-right-via-fold-left to preserve stack; check fold-r-via-fold-l variations above
(defn map-fpins-1 [xs f]
  (fold-r-via-fold-l xs '() #(cons (f %) %2)))
;2) using local mutation - but mutation is not visible outside function
;that is to quote fpins book: "mutation isn't observable outside the function"
;here is my translation of it by means of transient list of clojure world :)
(defn map-fpins-2 [xs f]
  (loop [[x & others :as original] xs acc (transient [])]   ; clojure does not support transient list!
    (if (nil? original)
      (persistent! acc)                                     ;after mutating make it persistent!
      (recur others (conj! acc (f x))))))                   ; for large collections it is faster :)
;-----------------------------------------------------------------------

;3.19 - this exercise for me was really eyes-opening :) to use fold-r
;although I was thinking about utilizing fold-r I did with explicit loop :)
;mine
(defn my-filter [xs f]
  (loop [[h & t :as original] xs acc '()]
    (if (nil? original)
      (my-reverse acc)
      (if (f h)
        (recur t (cons h acc))
        (recur t acc)))))
;fpins - definitely eyes-opening or better said: hitting into forehead :)
;variations:
;0) pure fold-r - same discussion as for map applies here: fold-r as
; implemented here is not stack safe
(defn filter-fpins [xs f]
  (fold-right xs '() #(if (f %)
                       (cons % %2)
                       %2)))
;1) fold-r-via-fold-l
(defn filter-fpins-1 [xs f]
  (fold-r-via-fold-l-1-fpins xs '() #(if (f %)
                                      (cons % %2)
                                      %2)))
;2) via mutable buffer - clojure equivalent would be via transient vector
;as to repeat clojure does not support transient list
;-----------------------------------------------------------------------

;3.20
;mine
(defn flat-map [xs f]
  (fold-r-via-fold-l xs '() (fn [x acc]
                              (let [tx (f x)]
                                (if (and (coll? tx) (not-empty tx))
                                  (list-concat [tx acc])
                                  (if (not (nil? tx))
                                    (cons tx acc)
                                    acc))))))               ;WHOA! :)
;my solution is more general though, allowing transforming function not to
;return list - this function is heavily "cludged" to accomodate solution to 3.21 and
;to keep ... "generality"... clap! clap! clap! :) (ironical self-criticsm)

;fpins - strictly assumes transforming function returns list thus it is simpler
(defn flat-map-fpins [xs f]
  (list-concat-fpins (map-fpins-1 xs f)))
;-----------------------------------------------------------------------

;3.21
;mine
(defn filter-via-flat-map [xs f]
  (flat-map xs (fn [x]
                 (if (f x)
                   (list x)
                   nil))))
;fpins
(defn filter-via-flat-map-fpins [xs f]
  (flat-map-fpins xs #(if (f %) (list %) nil)))
;-----------------------------------------------------------------------

;3.22
;mine
(with-test
  (defn add-lists [xs ys]
    (letfn [(go [xs ys acc]
                (if (or (empty? xs) (empty? ys))
                  (list* acc)
                  (recur (next xs) (next ys) (conj acc (+ (first xs) (first ys))))))]
      (go xs ys [])))
  (is (= '(5 7 9) (add-lists '(1 2 3) '(4 5 6)))))
;fpins
(with-test
  (defn add-lists-fpins [xs ys]
    (if (or (nil? xs) (nil? ys))
      nil
      (let [[h1 & t1] xs [h2 & t2] ys]
        (cons (+ h1 h2) (add-lists-fpins t1 t2)))))
  (is (= '(5 7 9) (add-lists-fpins '(1 2 3) '(4 5 6)))))
;-----------------------------------------------------------------------

;3.23
;mine
(with-test
  (defn zip-with [xs ys f]
    (letfn [(go [xs ys acc]
                (if (or (empty? xs) (empty? ys))
                  (list* acc)
                  (recur (next xs) (next ys) (conj acc (f (first xs) (first ys))))))]
      (go xs ys [])))
  (is (= '(-3 -3 -3) (zip-with '(1 2 3) '(4 5 6) -))))
;fpins
(with-test
  (defn zip-with-fpins [xs ys f]
    (if (or (nil? xs) (nil? ys))
      nil
      (let [[h1 & t1] xs [h2 & t2] ys]
        (cons (f h1 h2) (zip-with-fpins t1 t2 f)))))
  (is (= '(4 10 18) (zip-with-fpins '(1 2 3) '(4 5 6) *))))
;-----------------------------------------------------------------------

;3.24
;mine
(defn has-subsequence [sup sub]                             ;lesson learned - always use empty? to check collection
  (letfn [(starts-with [ssub ssup]
                       (loop [i2sub ssub i2sup ssup acc-sup '()]
                         (if (or (empty? i2sub) (empty? i2sup))
                           (= (reverse ssub) acc-sup)
                           (let [[h1 & t1] i2sup]
                             (recur (next i2sub) t1 (conj acc-sup h1))))))]
    (loop [isup sup res '()]
      (let [pass (starts-with sub isup)]
        (if (empty? isup)
          (not (every? false? res))
          (recur (next isup) (conj res pass)))))))
;fpins - perhaps verbosity below would disappear if pattern match is used
(defn starts-with [l prefix]
  (if (nil? prefix)
    true
    (let [[h1 & t1] l [h2 & t2] prefix]
      (if (= h1 h2)
        (recur t1 t2)
        false))))
(defn has-subsequence-fpins [sup sub]
  (if (empty? sup)
    (empty? sub)
    (if (starts-with sup sub)
      true
      (let [[h & t] sup]
        (if (and (not (nil? h)) (not (empty? t)))
          (recur t sub)
          false)))))
;fpins - rewritten with clojure core.match

(with-test
  (defn starts-with-revisited [l prefix]
    (match [l prefix]
           [_ ([] :seq)] true                               ;[_ ([] :seq :<< empty?)] true was here before but this is more concise :)
           [([h & t] :seq) ([h2 & t2] :seq)] (if (= h h2)
                                               (starts-with-revisited t t2)
                                               false)
           :else false))
  (is (true? (starts-with-revisited '(1 2 3 4) '(1 2 3))))
  (is (true? (starts-with-revisited '(1 2 3 4) '(1 2 3))))
  (is (false? (starts-with-revisited '(1 2 3 4) '(1 3))))
  (is (false? (starts-with-revisited '(1 2 3 4) '(1 2 3 4 5)))))

(with-test
  (defn has-subsequence-revisited [sup sub]
    (match [sup]
           [([] :seq)] (empty? sub)                         ; same here as above
           [_ :guard #(starts-with-revisited % sub)] true
           [([_ & t] :seq)] (recur t sub)))
  (is (true? (has-subsequence-revisited '(1 2 3 4) '(1 2 3))))
  (is (true? (has-subsequence-revisited '(1 2 3 4) '(2 3 4))))
  (is (false? (has-subsequence-revisited '(1 2 3 4) '(3 4 5))))
  (is (false? (has-subsequence-revisited '(1 2 3 4) '(1 2 3 4 5)))))
;-----------------------------------------------------------------------

;3.25
;mine
;First of all - clojure doesn't have Scala's traits so example can't
;be matched 1-1 - functions and maps are used instead, so what I am doing here
;is actually hybrid of JOC and FPINS (and JOC example is tweaked to more
;match FPINS way (explicit branch selection as in FPINS, unlike "inferred"
;branch depending on val
(defn xconj [tree branch val]
  (cond
    (nil? tree) {:val val, :L nil, :R nil}
    (nil? branch) tree
    (= :L branch) {:val (:val tree)
                   :L   (xconj (:L tree) branch val)
                   :R   (:R tree)}
    :else {:val (:val tree)
           :L   (:L tree)
           :R   (xconj (:R tree) branch val)}))

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(defn t-size [t]
  (letfn [(go [t acc]
              (if (nil? t)
                acc
                ;my solution is wrong here :| I am passing same acc to both
                ;left and right path walk
                (+ (go (:L t) (inc acc)) (go (:R t) (inc acc)))))]
    (+ (go (:L t) 0) (go (:R t) 0))))
;fpins - no pattern matching
(defn t-size-fpins [t]
  (if (and (nil? (:R t)) (nil? (:L t)))
    1
    (+ 1 (t-size-fpins (:R t)) (t-size-fpins (:L t)))))
;fpins - with pattern matching
(defn t-size-fpins-revisited [t]
  (match [t]
         [(:or {:val _ :R nil :L nil} nil)] 1
         [{:val _ :R r :L l}] (+ 1 (t-size-fpins-revisited r) (t-size-fpins-revisited l))))
;-----------------------------------------------------------------------

;3.26
;mine
(defn t-max [t]
  (letfn [(go [t acc]
              (if (nil? t)
                acc
                (let [val (:val t) new-max (if (nil? acc) val (max val acc))]
                  (max (go (:R t) new-max) (go (:L t) new-max)))))]
    (go t nil)))
;fpins - what is cumbersome here is probably necessity of covering all cases when matching
;over map but that is limitation of Clojure (or perhaps my own as I am not familiar currently
;with better options) as Scala variant of FPINS is utilizing matching over two case classes which
;are extending same Trait. Nevertheless, here it is, how I translated it
;not though that order of patterns here matters very much as e.g. pattern {:val :L l :R r} captures
;when l and r are nils as well so explicit match of nil must come before
(defn t-max-fpins [t]
  (match t
         {:val v :L nil :R nil} v
         {:val v :L nil :R r} (max v (t-max-fpins r))
         {:val v :L l :R nil} (max v (t-max-fpins l))
         {:val v :L l :R r}  (max v (t-max-fpins l) (t-max-fpins r))))
;-----------------------------------------------------------------------

;3.27
;mine
(defn t-depth [t]
  (letfn [(go [t depth depths]
              (if (nil? t)
                depths
                (clojure.set/union
                  (conj (go (:L t) (inc depth) depths) depth)
                  (conj (go (:R t) (inc depth) depths) depth))))]
    (apply max (go t 1 #{}))))
;fpins
;-----------------------------------------------------------------------

;3.28
;mine
(defn t-map [t f]
  (letfn [(go [t branch new-t]
              (if (nil? t)
                new-t
                (go (branch t) branch (xconj new-t branch (f (:val t))))))]
    (go (:L t) :L (go (:R t) :R (xconj nil nil (f (:val t)))))))

