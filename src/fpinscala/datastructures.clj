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
  (loop [[x & others] xs acc z]
    (if (nil? others)
      (f x acc)
      (recur others (f x acc)))))
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
;-----------------------------------------------------------------------

;3.11
;mine
(defn sum-fl [xs]
  (fold-left xs 0 +))
(defn product-fl [xs]
  (fold-left xs 1 *))
(defn len-fl [xs]
  (fold-left xs  0 (fn [_ x] (+ 1 x))))
;fpins
;All are basically same :)
;-----------------------------------------------------------------------

;3.12
;mine
(defn my-reverse [xs]
  (fold-left xs '() cons))
(defn my-reverse2 [xs]
  (fold-right xs [] #(conj %2 %)))                           ;This is
;way to do it with fold-right as list does not support adding at the end
;fpins
;It is done via f-l as in my first attempt
;-----------------------------------------------------------------------