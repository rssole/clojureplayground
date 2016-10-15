(ns amazon.interview-preparation)

(defn- smallest [xs]
  (reduce #(let [[e im ci] %]
            (if (<= %2 e)
              [%2 ci (inc ci)]
              [e im (inc ci)])) [(first xs) 0 0] xs))

(defn selection-sort [s]
  (loop [xs s acc []]
    (if (seq xs)
      (let [[x i] (smallest xs)
            [l r] (split-at i xs)]
        (recur (concat l (rest r)) (conj acc x)))
      acc)))

(defn binary-search [x xs]
  ;todo come on come on :)
  )
