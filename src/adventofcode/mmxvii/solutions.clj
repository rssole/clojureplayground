(ns adventofcode.mmxvii.solutions)

(defn ch->int [c]
  (Character/getNumericValue ^char c))

(defn- crf [acc xs]
  (+ acc (* (ch->int (first xs)) (dec (count xs)))))

(defn- captcha-1st [c]                                      ;1st attempt
  (let [ps (partition-by identity c)
        rps (if (= (last c) (first c))
              (cons (concat (first ps) (last ps)) (rest (butlast ps)))
              ps)]
    (reduce crf 0 (filter #(> (count %) 1) rps))))

(defn captcha [c]                                           ;Much simpler option after verifying with previous
  (let [x (first c)
        s (if (= x (last c)) (ch->int x) 0)]
    (reduce + s
            (map (comp ch->int first)
                 (filter (fn [[a b]] (= a b))
                         (partition 2 1 c))))))

(defn captcha2 [c]
  (let [n (count c)
        d (/ n 2)]
    (map-indexed #(vector %1 %2) c)))