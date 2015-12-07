(ns clojureplayground.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(foo "Trla baba lan")





;(print ((fn [x]
;         (reverse
;           (loop [cnt (- x 2) acc '(1 1)]
;             (if (zero? cnt)
;               acc
;               (recur (dec cnt) (conj acc (reduce + (take 2 acc)))))))) 6))

(defn fib [x]
  (reverse
    (loop [cnt (- x 2) acc '(1 1)]
      (if (zero? cnt)
        acc
        (recur (dec cnt) (conj acc (reduce + (take 2 acc))))))))

(print (take 4 (fib 10)))

(fn [[a b] _] [b (+ a b)])

(fn [n]
  (map first (reductions
               (fn [[a b] _] [b (+ a b)]) [1 1] (range 1 n))))

(def input '((1 2) 3 [4 [5 6]]))

;(defn my-flatten [coll]
;  (for [x coll]
;    (loop [elem x acc '()]
;      (if (coll? elem) (recur elem acc) (conj acc elem)))))
;
;(print (my-flatten day1-input))

;4clojure infix calculator

((fn [x op y & others]                                      ;When I used "& rest" here and...
   (let [oacc (op x y)]
     (loop [xs others acc oacc]
       (let [curr (first xs) nxt (first (rest xs))]         ;... called (rest xs) here, I was getting NPE! OF COURSE YOU @#$!@##!
         (if (or (empty? xs) (nil? nxt))                    ;I SHADOWED CLOJURE'S REST FUNCTION WITH REST VARIADIC ARGS NAME!!!
           acc                                              ;LESSON: NEVER! NEVER! NEVER!!! USE CLOJURE'S FUNCTIONS NAMES FOR ANYTHING ELSE!
           (recur (drop 2 xs) (curr acc nxt))
           ))))) 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)


;Problem 80 - Perfect Numbers
((fn [x]
   (letfn [(gcd [a b] (if (= 0 b) a (gcd b (mod a b))))]
     (let [rng (range 1 (dec x))
           fcrs (set (for [y rng] (gcd y x)))]
       (= x (reduce + fcrs))))) 8128)

;Problem 60 - Sequence reductions
(defn my-reds
  ([f xs]
   (lazy-seq
     (my-reds f (first xs) (rest xs))))
  ([f i xs]
   (cons i
     (lazy-seq
       (when (seq xs)
         (my-reds f (f i (first xs)) (rest xs)))))))

