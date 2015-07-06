(ns fpinscala.datastructures)

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
(defn append [xs ys]
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
    (fold-left t h append)))
;fpins
;they did it via fold right what is even better (more concise that is)
(defn list-concat-fpins [ls]
  (fold-right ls nil append))
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
                                (if (coll? tx)
                                  (append tx acc)
                                  (cons tx acc))))))
;my solution is more general though, allowing transforming function not to
;return list
;fpins - strictly assumes transformin function returns list thus it is simpler
(defn flat-map-fpins [xs f]
  (list-concat-fpins (map-fpins-1 xs f)))
;-----------------------------------------------------------------------