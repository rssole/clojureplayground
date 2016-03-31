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


