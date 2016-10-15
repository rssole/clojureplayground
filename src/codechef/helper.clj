(ns codechef.helper)

;I need actually an sequence which will
;generate sides starting from [1 1 1] and will
;once predicate is hit try to reset position which
;broke the limit to the beginning and increase previous
;position and so on... until rightmost position breaks limit


(defn chef-and-triangles
  "Problem code:LTM40CD"
  [r]
  ())


#(let [[a b c] %
       s (/ (+ a b c) 2)
       p (Math/sqrt (* s (- s a) (- s b) (- s c)))
       r (/ p s)]
  (<= r 3))

(defn bla-bla-truc []
  (take 20 (map #(let [[a b c] %
                      s (double (/ (+ a b c) 2))
                      p (Math/sqrt (* s (- s a) (- s b) (- s c)))
                      r (/ p s)]
                 {:inradius r :permiter s :area p :sides %})
               (iterate #(update-in % [2] inc) [1 1 1]))))

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

(defn odometer-revisited [init limit]
  (fn [iv]
    (let [size (count iv) lc (inc (last iv))]
      (if (> lc limit)
        (-> (update-all-counters iv init limit)
            :odo
            rev-vec)
        (assoc iv (dec size) lc)))))