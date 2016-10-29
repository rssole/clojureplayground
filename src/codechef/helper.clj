(ns codechef.helper)

; I am not very proud of it :) but worked by solution 11654376 so kudos to author
(defn chef-and-triangles
  "Problem code:LTM40CD"
  []
  (binding [*unchecked-math* true]
    (let [r (Integer/parseInt (read-line))
          ts (for [x (range 1 (inc (* r (Math/sqrt 3))))
                   y (range (Math/max x (inc (- r x)))
                            (inc (/ (+ (* 2 r r) (* 2 r (Math/sqrt (+ (* r r) (* x x))))) (* 2 x))))
                   :let [u (* r r (+ x y))
                         v (- (* x y) (* r r))
                         z (when (> v 0) (/ u v))]
                   :when (and z (== (mod u v) 0) (<= x y) (<= x z) (<= y z))]
               [(+ x y) (+ x z) (+ y z)])]
      (println (count ts))
      (doseq [[a b c] (sort ts)]
        (println a b c)))))

(def rev-vec (comp vec reverse))

(defn- update-all-counters [iv init limit]
  (reduce #(let [{:keys [carry-over odo]} %]
            (if carry-over
              (let [x (inc %2) ol (> x limit)]
                (-> %
                    (assoc :carry-over ol)
                    (assoc :odo (conj odo (if ol init x)))))
              (assoc % :odo (conj odo %2))))
          {:carry-over true :odo [init]}
          (-> iv
              butlast
              rev-vec)))

(defn an-odometer
  "Creates odometer counter function with given bounds - limit is inclusive"
  [init limit]
  (fn [iv]
    (let [size (count iv) lc (inc (last iv))]
      (if (> lc limit)
        (-> (update-all-counters iv init limit)
            :odo
            rev-vec)
        (assoc iv (dec size) lc)))))

(defn odometer-generator [size lower upper]
  (fn [x]
    (if (= upper (last x))
      (let [cnt (count (take-while #(= % upper) (reverse x)))]
        (vec (map-indexed #(let [pos (- (dec size) cnt)]
                            (cond
                              (= pos %) (inc %2)
                              (< % pos) %2
                              (> % pos) lower))
                          x)))
      (assoc x (dec size) (inc (last x))))))
