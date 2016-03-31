(ns clojureplayground.codingame-helper)

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
(let [inp ["00" "0."]
      node? #(when (= \0 (.charAt %2 %)) %)
      nright #(let [x (.indexOf % %2)]
               (if (>= x 0) [x %3] [-1 -1]))
      nbottom #()]
  (map-indexed (fn [y row]
                 (map-indexed (fn [x c]
                                (when (node? c x)
                                  (let [ntb (nbottom (drop (inc y) inp))
                                        ntr (nright row (inc x) y)]))) row)) inp))









