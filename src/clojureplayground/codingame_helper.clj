(ns clojureplayground.codingame-helper
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
(defn node-to-cut [aloc links]
  (if-let [fc (some #(let [{:keys [f t severed]} %]
                      (when (and (= f aloc) (zero? t) (false? severed))
                        %)) links)]
    fc
    (first (filter #(let [{:keys [f severed]} %]
                     (and (= f aloc) (false? severed))) links))))

(let [[_ L E] (map s->i (spws (read-line)))
      [links egs] (split-at (* 2 L) (mapcat #(map s->i (spws %)) (repeatedly (+ E L) read-line)))
      lks (vec
            (map-indexed
              #(let [[from to] %2]
                {:i % :f from :t to :severed false})
              (partition 2 links)))]
  (println lks)
  (loop [lks0 lks]
    (let [SI (read) {:keys [i f t] :as fnd} (node-to-cut SI lks0)]
      (println f t)
      (recur (update-in lks0 [i :severed] #(identity %2) true)))))


;(println (mapcat #(clojure.string/split % #"\s") (repeatedly 3 read-line)))