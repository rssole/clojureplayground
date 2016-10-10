(ns amazon.interview-preparation)

(defn- smallest [xs]
  (reduce #(let [[e im ci] %]
            (if (<= %2 e)
              [%2 ci (inc ci)]
              [e im (inc ci)])) [(first xs) 0 0] xs))

(defn selection-sort [s]
  (loop [xs s acc []]
    (if (seq xs)
      (let [[x i] (smallest xs)]
        (recur (concat (take i xs) (drop (inc i) xs)) (conj acc x)))
      acc)))
