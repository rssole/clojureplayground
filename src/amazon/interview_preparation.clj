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

; todo: try to implement it via loop/recur to preserve stack space
(defn binary-search [x xs]
  (letfn [(go [ixs start end]
            (let [im (int (/ (count ixs) 2))
                  [l [h & r]] (split-at im ixs)
                  rstart (+ start im)]
                (cond
                  (= h x) rstart
                  (and (seq r) (> x h)) (go r (inc rstart) end)
                  (and (seq l) (< x h)) (go l start im)
                  :else nil)))]
    (go xs 0 (dec (count xs)))))

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
  (loop [a arr dups '()]
    (let [[h & t] a]
      (if t
        (recur t (if (some #{h} t) (conj dups h) dups))
        dups))))

; Both above are ridiculously slow

(defn all-duplicates-leetcode-port
  "Clojure port of accepted solution I did with Java 8"
  [nums]
  (sort
    (mapv first (filter
                 #(> (second %) 1)
                 (reduce #(if (% %2)
                            (update % %2 inc)
                            (assoc % %2 1))
                         {}
                         nums)))))