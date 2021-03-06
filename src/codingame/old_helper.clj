(ns codingame.old_helper
  (:gen-class))

(defn direction [adj opp]
  (if (neg? adj)
    (if (neg? opp) 3 2)
    (if (neg? opp) 1 0)))

(defn co [x y dir] {:x x :y y :dir dir})

(defn allowed-lines [theta]
  (first
    (for [t [[#(<= % (/ Math/PI 8)) [(co 1 0 "E") (co 1 0 "E") (co -1 0 "W") (co -1 0 "W")]]
             [#(and (> % (/ Math/PI 8)) (<= % (* Math/PI 0.375))) [(co 1 1 "SE") (co 1 -1 "NE") (co -1 1 "SW") (co -1 -1 "NW")]]
             [#(and (> % (* Math/PI 0.375)) (<= % (/ Math/PI 2))) [(co 0 1 "S") (co 0 -1 "N") (co 0 1 "S") (co 0 -1 "N")]]]
          :when ((first t) theta)] (second t))))

(defn thor-move2 [adj opp theta]
  ((allowed-lines theta) (direction adj opp)))

;APU: Init Phase
#_(let [inp ["00" "0."]
        neighr (fn [x y row]
                 (let [nx (.indexOf row "0" (inc x))]
                   (if (>= nx 0) [nx y] [-1 -1])))
        neighb (fn [x y in]
                 (reduce (fn [[_ _ cnt] i]
                           (if (= \0 (.charAt i x))
                             (reduced [x cnt])
                             [-1 -1 (inc cnt)])) [-1 -1 (inc y)] (drop (inc y) in)))]
    (map #(clojure.string/join " " %)
         (filter some?
                 (apply concat
                        (map-indexed (fn [y row]
                                       (map-indexed (fn [x c]
                                                      (when (= c \0)
                                                        (let [[x1 y1] (neighr x y row)
                                                              [x2 y2] (neighb x y inp)] [x y x1 y1 x2 y2]))) row)) inp)))))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(def s->i #(Integer/parseInt %))
(def spws #(clojure.string/split % #"\s"))
(defn find-node-by [aloc pred]
  (fn [node]
    (let [{:keys [f t] :as n} node]
      (pred n aloc f t))))
(defn node-to-cut [aloc links egs]
  (if-let [fc (some (find-node-by aloc #(when (or
                                                (and (= %2 %3) (egs %4))
                                                (and (= %2 %4) (egs %3))) %)) links)]
    fc
    (first (filter (find-node-by aloc #(or (egs %4) (egs %3))) links))))
(defn skynet-virus [& args]
  (let [[_ L E] (map s->i (spws (read-line)))
        [links egws] (split-at (* 2 L) (mapcat #(map s->i (spws %)) (repeatedly (+ E L) read-line)))
        lks (mapv (fn [[from to]]
                    {:f from :t to})
                  (partition 2 links))
        egs (set egws)]
    (reduce (fn [ls si]
              (let [{:keys [f t]} (node-to-cut si ls egs)]
                (println f t)ls))
            lks
            (repeatedly read))))

;(println (mapcat #(clojure.string/split % #"\s") (repeatedly 3 read-line)))
#_[{:i 0, :f 11, :t 6, :severed false}]
 {:i 1, :f 0, :t 9, :severed false}
 {:i 2, :f 1, :t 2, :severed false}
 {:i 3, :f 0, :t 1, :severed false}
 {:i 4, :f 10, :t 1, :severed false}
 {:i 5, :f 11, :t 5, :severed false}
 {:i 6, :f 2, :t 3, :severed false}
 {:i 7, :f 4, :t 5, :severed false}
 {:i 8, :f 8, :t 9, :severed false}
 {:i 9, :f 6, :t 7, :severed false}
 {:i 10, :f 7, :t 8, :severed false}
 {:i 11, :f 0, :t 6, :severed false}
 {:i 12, :f 3, :t 4, :severed false}
 {:i 13, :f 0, :t 2, :severed false}
 {:i 14, :f 11, :t 7, :severed false}
 {:i 15, :f 0, :t 8, :severed false}
 {:i 16, :f 0, :t 4, :severed false}
 {:i 17, :f 9, :t 10, :severed false}
 {:i 18, :f 0, :t 5, :severed false}
 {:i 19, :f 0, :t 7, :severed false}
 {:i 20, :f 0, :t 3, :severed false}
 {:i 21, :f 0, :t 10, :severed false}
 {:i 22, :f 5, :t 6, :severed false}

; Recurring decimals puzzle
#_(fn [n]
  (let [[l r] (clojure.string/split (str (.doubleValue (/ 1 n))) #"\.")]
    (if-let [ss (last
                  (flatten
                    (filter #(and (>= (count %) 2) (> (count (first (last %))) 1))
                            (map #(re-seq (re-pattern (str "(" % ")")) r)
                                 (reductions #(str % %2) "" r)))))])))


(defn -main [& args]
  (let [n (read) [l r] (clojure.string/split (str (.doubleValue (/ 1 n))) #"\.0+")]
    (binding [*out* *err*](println n))
    (if-let [ss (last
                  (flatten
                    (filter #(and (>= (count %) 2) (> (count (first (last %))) 1))
                            (map #(re-seq (re-pattern (str "(" % ")")) r)
                                 (reductions #(str % %2) "" r)))))]
      (println (str l "." "(" ss ")"))
      (cond
        (.contains r "E-") (println "0.")
        (clojure.string/starts-with? r "00")
        (println (str l "." "(" r ")"))))))

;todo IDEA! 1428571428571428571428571428571429
;go and build subseq 1 by 1 until you find that
;string yielded by dropping subseq len of chars
;begins with already built subseq...