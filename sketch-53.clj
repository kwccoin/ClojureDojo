; #53
; Longest Increasing Sub-Seq
;  
; Difficulty:	Hard
; Topics:	seqs
; 
; Given a vector of integers, find the longest consecutive
; sub-sequence of increasing numbers.
; If two sub-sequences have the same length, use the one
; that occurs first.
; An increasing sub-sequence must have a length of 2 or greater to qualify.
;
; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
; (= (__ [5 6 1 3 2 7]) [5 6])
; (= (__ [2 3 3 4 5]) [3 4 5])
; (= (__ [7 6 5 4]) [])
; 

;; Breaks on [2 3 3 4 5]
;; http://www.4clojure.com/problem/53#prob-title
(defn subseq- [coll]
  (partition-by (fn [x] (= 1 (- (second x) (first x))))
                (filter #(< (first %) (second %))
                        (partition 2 1 coll))))

;; Example
(def test-vecs [[1 0 1 2 3 0 4 5] [5 6 1 3 2 7] [2 3 3 4 5] [7 6 5 4]])

(vector
 (for [i test-vecs]
   (vec (set (flatten (first (subseq- i)))))))

;; Result: [([0 1 2 3] [5 6] [2 3 4 5] [])]

;; hyone's solution to Longest Increasing Sub-Seq
;; https://4clojure.com/problem/53

(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
;

; #53 - Appendix
; is-sequential? predicate function

; Fast:
(defn iseq? [numbers]
  (loop [i 1 s (seq numbers)]
    (if s
      (if (== (first s) i)
        (recur 
          (inc i) 
          (next s)))
       true)))

; Possibly-unreadably clever:
  (defn iseq?2 [numbers]
    (and
      (= (count numbers) (count distinct numbers))
      (every? identity (map #(get (into [] numbers) (dec %) nil) numbers))))

; Simple:
(defn iseq?3 [numbers]
  (= numbers (range 1 (inc (count numbers)))))

(defn f [xs] 
  (every? 
    #(apply = %) 
    (map vector xs (iterate inc 1))))

(defn f [xs] 
  (every? 
    true? 
    (map = xs (iterate inc 1))))

; from dnolen
(defn conj-if-sequential
  ([] [])
  ([a] a)
  ([a b] (let [a (if (vector? a) a [a])]
           (if (= (inc (last a)) b)
             (conj a b)
             a))))

(reduce conj-if-sequential [2 3 4 6 8 1])

(defn sequential-seqs
  ([] [])
  ([a] a)
  ([a b] (let [n (last (last a))]
           (if (and n (= (inc n) b))
             (update-in a [(dec (count a))] conj b)
             (conj a [b])))))

(defn largest
  ([] nil)
  ([a] a)
  ([a b] (if (> (count b) (count a)) b a)))

(reduce largest (reduce sequential-seqs [] [2 3 4 6 8 1 4 5 6 7 8 9 13]))


#_(
; user=> (doc partition)
; -------------------------
; clojure.core/partition
; ([n coll] [n step coll] [n step pad coll])
;   Returns a lazy sequence of lists of n items each, at offsets step
;   apart. If step is not supplied, defaults to n, i.e. the partitions
;   do not overlap. If a pad collection is supplied, use its elements as
;   necessary to complete last partition upto n items. In case there are
;   not enough padding elements, return a partition with less than n items.
; nil
; user=> (doc filter)
; -------------------------
; clojure.core/filter
; ([pred coll])
;   Returns a lazy sequence of the items in coll for which
;   (pred item) returns true. pred must be free of side-effects.
; nil
; user=> (doc reduce)
; -------------------------
; clojure.core/reduce
; ([f coll] [f val coll])
;   f should be a function of 2 arguments. If val is not supplied,
;   returns the result of applying f to the first 2 items in coll, then
;   applying f to that result and the 3rd item, etc. If coll contains no
;   items, f must accept no arguments as well, and reduce returns the
;   result of calling f with no arguments.  If coll has only 1 item, it
;   is returned and f is not called.  If val is supplied, returns the
;   result of applying f to val and the first item in coll, then
;   applying f to that result and the 2nd item, etc. If coll contains no
;   items, returns val and f is not called.
; nil
; user=> (doc flatten)
; -------------------------
; clojure.core/flatten
; ([x])
;   Takes any nested combination of sequential things (lists, vectors,
;   etc.) and returns their contents as a single, flat sequence.
;   (flatten nil) returns nil.
; nil
; user=> (doc count)
; -------------------------
; clojure.core/count
; ([coll])
;   Returns the number of items in the collection. (count nil) returns
;   0.  Also works on strings, arrays, and Java Collections and Maps
; nil
; user=> (doc distinct)
; -------------------------
; clojure.core/distinct
; ([coll])
;   Returns a lazy sequence of the elements of coll with duplicates removed
; nil
; user=> (doc ffirst)
; -------------------------
; clojure.core/ffirst
; ([x])
;   Same as (first (first x))
; nil
; user=> 
)