(ns clojureplayground.codingame-helper)

(defn thor-move1 [adj opp theta]
  (if (>= adj 0)
    (if (>= opp 0)
      (cond
        (<= theta (/ Math/PI 8)) {:x 1 :y 0 :dir "E"}
        (and (> theta (/ Math/PI 8)) (<= theta (* Math/PI 0.375))) {:x 1 :y 1 :dir "SE"}
        (and (> theta (* Math/PI 0.375)) (<= theta (/ Math/PI 2))) {:x 0 :y 1 :dir "S"})
      (cond
        (<= theta (/ Math/PI 8)) {:x 1 :y 0 :dir "E"}
        (and (> theta (/ Math/PI 8)) (<= theta (* Math/PI 0.375))) {:x 1 :y -1 :dir "NE"}
        (and (> theta (* Math/PI 0.375)) (<= theta (/ Math/PI 2))) {:x 0 :y -1 :dir "N"}))
    (if (>= opp 0)
      (cond
        (<= theta (/ Math/PI 8)) {:x -1 :y 0 :dir "W"}
        (and (> theta (/ Math/PI 8)) (<= theta (* Math/PI 0.375))) {:x -1 :y 1 :dir "SW"}
        (and (> theta (* Math/PI 0.375)) (<= theta (/ Math/PI 2))) {:x 0 :y 1 :dir "S"})
      (cond
        (<= theta (/ Math/PI 8)) {:x -1 :y 0 :dir "W"}
        (and (> theta (/ Math/PI 8)) (<= theta (* Math/PI 0.375))) {:x -1 :y -1 :dir "NW"}
        (and (> theta (* Math/PI 0.375)) (<= theta (/ Math/PI 2))) {:x 0 :y -1 :dir "N"}))
    ))

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











