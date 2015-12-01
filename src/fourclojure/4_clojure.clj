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