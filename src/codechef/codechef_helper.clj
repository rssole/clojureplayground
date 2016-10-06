(ns codechef.codechef-helper)

(defn chef-and-triangles
  "Problem code:LTM40CD"
  [r]
  (take 10 (filter
     #(let [[a b c] %
            s (double (/ (+ a b c) 2))
            area (Math/sqrt (* s (- s a) (- s b) (- s c)))
            ir (double (/ area s))]
       (= (Math/round ir) r))
     (for [a (iterate inc 1) b (iterate inc 1) c (iterate inc 1)]
       (do
         [a b c])))))
