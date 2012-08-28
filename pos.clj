;The Joy of Clojure
; Chap 5.7
;The pos function.

;The pos function work on any composite type returning indices corresponding to some value.
;Return a numerical index for sequential collections or associated key for maps and sets
;Otherwise return nil

(defn pos [e coll]
  (let [cmp (if (map? coll)
              #(= (second %1) %2)
              #(= %1 %2))]
    (loop [s coll idx 0]
      (when (seq s)
        (if (cmp (first s) e)
          (if (map? coll)
            (first (first s))
            idx)
          (recur (next s) (inc idx)))))))

;take a collection and return a sequence of pairs ([index1 value1] [index2 value2] ...)

(defn index [coll]
  cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

;with the function index, finding the position indices for the desired value is trivial:

(defn pos [e coll]
    (for [[i v] (index coll) :when (= e v)] i))

;the last version of pos implement a predicate function as argument to archieve the ideal level of flexibility.
(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

;; demonstrating the power of iterate
;; to generate the Fibonacci sequence
user=> (def fib (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
#'user/fib

user=> (take 10 fib)
(1 1 2 3 5 8 13 21 34 55)

; back to studying!
