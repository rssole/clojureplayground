(ns fourclojure.4-clojure)

;problem 69
((fn [f & ms]
   (into {} (for [[k v]
                  (group-by first
                            (for [m ms [k v] m]
                              [k v]))]
              {k (reduce f (map second v))}))) concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})

;problem 70
((fn [s]
   (let [[a & others] (clojure.string/split s (re-pattern "-"))]
     (apply str a
            (map #(let [[x & ys] %]
                    (apply str (Character/toUpperCase x) ys)) others)))) "multi-word-key")

;problem 75
((fn [x] (letfn [(gcd [a b]
                   (loop [ia a ib b]
                     (if (= ib 0)
                       ia
                       (recur ib (mod ia ib)))))]
           (count (filter #(= 1 (gcd % x)) (range 1 (inc x)))))) 99)

;problem 86
((fn [x]
   (letfn [(square-ch [y] (let [n (Integer/parseInt (str y))] (* n n)))]
     (loop [s (str x) ctrl #{}]
       (let [s2 (reduce + (map #(square-ch %) s))]
         (println s2)
         (cond
           (= s2 1) true
           (ctrl s2) false
           :else (recur (str s2) (conj ctrl s2))))))) 7)