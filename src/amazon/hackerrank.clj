(ns amazon.hackerrank)

(def file-lines (line-seq (clojure.java.io/reader "/ext/sdb2/test_cases_r00t3e5nj8/input001.txt")))

(def affinity (Float/parseFloat (first file-lines)))
(def items-count (Integer/parseInt (second file-lines)))

(defn process-single-line [s]
  (let [[iteml itemr aff] (clojure.string/split s #" ")]
    {:il iteml :ir itemr :aff (Float/parseFloat aff)}))

(def records (doall
               (map process-single-line
                    (drop 2 file-lines))))

(def all-items (doall
                 (reduce #(conj % (:il %2) (:ir %2)) #{} records)))

(defn find-clusters [items input]
  (for [item items]
    (loop [item-cluster [item] curr item recs input]
      (if (seq recs)
        (let [r (first recs)]
          (if (and (= curr (:il r)) (> (:aff r) affinity))
            (recur (conj item-cluster (:ir r)) (:ir r) (rest recs))
            (recur item-cluster curr (rest recs))))
        item-cluster))))


(println
  (ffirst
    (sort #(> (count %) (count %2))
          (doall (find-clusters all-items records)))))







