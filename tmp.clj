; #51
; Advanced Destructuring
;  
; Difficulty:	Easy
; Topics:	destructuring
; 
; Here is an example of some more sophisticated destructuring.
; (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
; 
; Solution:
; [1 2 3 4 5]

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

(defn is-consecutive? [coll]
  (cond
    ; (<=   (count coll) 1) :nil11
    ;(nil? (first (rest coll))) :nil0
    ; (>=   (first coll) (first (rest coll))) :nil1
    ; (<    (first coll) (first (rest coll))) (is-consecutive? (drop 2 coll))
     ))

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
