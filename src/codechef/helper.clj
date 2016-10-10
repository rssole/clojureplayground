(ns codechef.helper)

(defn chef-and-triangles
  "Problem code:LTM40CD"
  [r]
  (for [a (range 1 1000) b (range 1 1000) c (range 1 1000)
        :let [sides-sum (+ a b c)
              s (double (/ sides-sum 2))
              area (Math/sqrt (* s (- s a) (- s b) (- s c)))]
        :when (= r (Math/round (/ area s)))
        :while (<= sides-sum (/ (* area 2) r))]
    (do
      [a b c])))
