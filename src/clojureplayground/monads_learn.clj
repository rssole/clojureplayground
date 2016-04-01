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

;Monoids

;Simply put, Monoids describe types containing a binary function and an identity value.
;When applied to the identity value and a random value x, said function leaves its argument x untouched, returning it as a result.

;First - Haskell:
;class Monoid m where
; mempty :: m
; mappend :: m -> m -> m
; mconcat :: [m] -> m
; mconcat ms = foldr mappend mempty ms

;mempty - I started with a lie since mempty isn’t actually a function.
; You can think of it as a constant of the same type of the Monoid m.
; It is this monoid’s identity value.
;mappend - A poorly named function,
; mappend is the binary function I mentioned earlier.
; It receives two arguments of type m and returns a value of type m
;mconcat - It receives a list of Monoids m and reduces them to a single Monoid of type m.
; What’s interesting about this snippet is that the Monoid type class provides a
; default implementation for mconcat: it simply calls foldr with the binary function mappend,
; a starting value of mempty and the list of Monoid values ms

(defn plus-monoid
  ([]
   0)
  ([a b]
   (+ a b)))

(defn list-monoid
  ([]
   '())
  ([a b]
   (concat a b)))

;Monads :) TADAAAAAAAAAAAA! :)
;Quote: "The point is: when we combine Functors/Applicatives/Monads,
; we carry their context with us to the end - they are essentially sequenced together."

;First - Haskell:
;class Monad m where

;  return :: a -> m a
;Responsible for wrapping a value of type a into a minimum context
; Monad that yields a value of type a - referred to as a monadic value.

;  (>>=) :: m a -> (a -> m b) -> m b AKA "bind"
;Function of two arguments. The first is a monadic value of type a and
; the second is a function that receives a value of type a and
; returns a monadic value of type m b which is also the overall result of the function.

;  (>>) :: m a -> m b -> m b AKA "then" and below is it's default implementation
;    x >> y = x >>= \_ -> y
;This function receives two monads, m a and m b, and returns a monad of type m b.
; It is generally used when you’re interested in the side effects - the context - carried out by the monad m a
; but doesn’t care about the value a it yields. It’s rarely implemented in specific monads
; because the type class provides a default implementation:
; It applies bind to the monad x and a function that ignores its argument (\_ -> y) - which
; by convention is represented by an underscore - and simply yields the monad y: that’s the final result of the computation.

; Hint: there is "fail" function as well but it is omitted for clarity purposes

(def maybe-monad {
                  :return (fn [v] v)
                  :bind   (fn [mv f]
                            (if mv
                              (f mv)
                              nil))})

;For the maybe monad, all its context needs to represent is a single value or
; the absence of value. We do this inside bind by checking if the monadic value mv is nil.
; If it isn’t, we apply f to it, which will yield another monadic value.
; If, on the other hand, mv IS nil, we just return nil, bypassing the function application entirely.

;return, as we saw, wraps a value into a minimal monad. In this case this is the value itself, so we just return it untouched.