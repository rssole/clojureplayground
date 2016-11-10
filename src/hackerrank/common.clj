(ns hackerrank.common
  (:require [clojure.string :as cs]))

(defn str->int [s]
  (Integer/parseInt s))

(defn str->ints
  "Turns space separated string of numbers into sequence of them"
  [s]
  (map str->int (cs/split s #" ")))

(defn with-test-cases-of-array [f]
  (let [cases-count (str->int (read-line))
        outcomes (repeatedly
                   cases-count
                   (fn []
                     (let [_ (str->int (read-line))
                           xs (str->ints (read-line))]
                       (f xs))))]
    (doseq [o outcomes]
      (if o
        (println "YES")
        (println "NO")))))
