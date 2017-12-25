(ns adventofcode.mmxvii.solutions)

(defn ch->int [c]
  (Character/getNumericValue ^char c))

(defn- crf [acc xs]
  (+ acc (* (ch->int (first xs)) (dec (count xs)))))

(defn captcha [c]
  (let [ps (partition-by identity c)
        rps (if (= (last c) (first c))
              (cons (concat (first ps) (last ps)) (rest (butlast ps)))
              ps)]
    (reduce crf 0 (filter #(> (count %) 1) rps))))