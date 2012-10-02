; #43
; Reverse Interleave
; Difficulty:	Medium
; Topics:	seqs
;
; Write a function which reverses the interleave process into x number of subsequences.
; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
; (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
; (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

(defn reverse-interleave
  [coll size]
  (apply 
    map 
      list (partition size coll)))
(reverse-interleave [1 2 3 4 5 6] 2)
(reverse-interleave (range 9) 3)
(reverse-interleave (range 10) 5)

; Solutions:
(fn [coll n]
  (apply map list (partition n coll)))
; austintaylor's solution:
(fn [s n]
  (map (fn [l] (map last l))
    (vals
      (group-by
        (fn [l] (mod (first l) n))
        (map-indexed list s)))))
; maximental's solution:
#(apply map list (partition %2 %))
; nikelandjelo's solution:
(fn r-interleave [s n]
  (let [shift-add (fn [s val]
    (cons (conj (last s) val) (butlast s)))]
      (reverse (reduce shift-add (repeat n []) s))))
; norman's solution:
(fn rleave [items n]
  (apply map list (partition n items)))

#_(
user=> (doc interleave)
-------------------------
clojure.core/interleave
([c1 c2] [c1 c2 & colls])
  Returns a lazy seq of the first item in each coll, then the second etc.
nil
user=> (doc reduce)
-------------------------
clojure.core/reduce
([f coll] [f val coll])
  f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called.
nil
user=> (doc apply)
-------------------------
clojure.core/apply
([f args* argseq])
  Applies fn f to the argument list formed by prepending args to argseq.
nil
user=> (doc partition)
-------------------------
clojure.core/partition
([n coll] [n step coll] [n step pad coll])
  Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items.
nil
user=> (doc list)
-------------------------
clojure.core/list
([& items])
  Creates a new list containing the items.
nil
user=> (doc map)
-------------------------
clojure.core/map
([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
  Returns a lazy sequence consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments.
nil
user=> 
)

;; If you were to try
;user=> (max [1 2 3])
; [1 2 3]
;; You would get '[1 2 3]' for the result. In this case, 'max' has received one
;; vector argument, and the largest of its arguments is that single vector.
;; If you would like to find the largest item **within** the vector, you would need
;; to use `apply`
;user=> (apply max [1 2 3])
;3
;; which is the same as (max 1 2 3)


; 44: Write a function which can rotate a sequence in either direction.
; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
(fn [n coll]
  (let [ntime (if (neg? n) (- n) n)
        lshift #(concat (rest %) [(first %)])
        rshift #(cons (last %) (drop-last %))]
    ((apply comp (repeat ntime (if (neg? n) rshift lshift))) coll)))
#(let [
       [l r] (split-at (mod % (count %2)) %2)]
    (concat r l))

; Source:
(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (map rest ss))))))))

; ---
