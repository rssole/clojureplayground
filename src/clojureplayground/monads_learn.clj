(ns clojureplayground.monads-learn)

;Functors
;In a nutshell Functors are things that can be mapped over.

;First: haskell stuff:

;class Functor f where
;  fmap :: (a -> b) -> f a -> f b

;fmap is a function that receives two arguments: the first one is a function
;that receives an argument of type a and returns a value of type b
;and the second is a Functor that contains value of type a - represented by 'f a'.
;The result of calling fmap is a Functor of same type - f - containing a value of type b,
;which is the result of applying the function to a

;Now - clojure
;Functor protocol
(defprotocol Functor
  (fmap [functor f] "Maps fn over the functor f"))

(defrecord List [wrapped]
  Functor
  (fmap [functor f]
    (->List (map f (:wrapped functor)))))

;fmap then is responsible for unwrapping the value contained in the list functor and mapping f over it.

;see appropriate tests for further elaboration

;Applicative Functors

;First - Haskell:
;class (Functor f) => Applicative f where
;  pure :: a -> f a
;  (<*>) :: f (a -> b) -> f a -> f b

;pure is a function that takes a value a and wraps it into a minimal Functor f.
;<*> is a function that takes two arguments:
; the first is a Functor f that wraps a function of type a -> b.
; The second argument is a Functor f that wraps a value - which could be a function! - of type a.
; The final result is a Functor f that wraps some value of type b - which was obtained
; by somehow applying the function (a -> b) to the Functor f a.

;; it dispatches on the record type since we could have implementations of pure for List, Maybe, Either etc...
(defmulti pure (fn [f _] f))

(defmethod pure List [_ v]
  "Wraps value v in a list"
  ;I've struggled for a while with this as I missed to wrap v into list (or in this case, under the hood - vector)
  (->List [v]))

;; it dispatches on the class of the Functor instance passed in the 1st argument
(defmulti <*> (fn [fs _] (class fs)))

(defmethod <*> List [fs xs]
  "Unwraps the functions in fs, applies them to the Functors in xs, wrapping the result at the end"
  (->List (for [f (:wrapped fs)
                x (:wrapped xs)]
            (f x))))