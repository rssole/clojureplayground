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

(defn all-duplicates
  "Leetcode problem 442. Find All Duplicates in an Array"
  [xs]
  (second
    (reduce (fn [a _]
              (let [[i acc] a
                    [l r] (split-at i xs)
                    n (first r)]
                (if (some #(= n %) (concat l (rest r)))
                  [(inc i) (conj acc n)]
                  [(inc i) acc]))) [0 #{}] xs)))

(defn all-duplicates-revisited
  "Leetcode problem 442. Revisited"
  [arr]
  (loop [a arr dups []]
    (let [[h & t] a]
      (if t
        (recur t (if (some #{h} t) (conj dups a) dups))
        (set dups)))))

; Both above are ridiculously slow

(defn all-duplicates-leetcode-port
  "Clojure port of accepted solution I did with Java 8"
  [nums]
  (map first (filter
               #(> (second %) 1)
               (reduce #(if (% %2)
                          (update % %2 inc)
                          (assoc % %2 1))
                       {}
                       nums))))