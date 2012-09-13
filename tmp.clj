
; 41. Drop Every Nth Item
;
; Difficulty:	Easy
; Topics:	seqs
; 
; Write a function which drops every Nth item from a sequence.
; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
; (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
; (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])

#_(
user=> (doc rem)
-------------------------
clojure.core/rem
([num div])
  remainder of dividing numerator by denominator.
nil
user=> (doc drop-last)
-------------------------
clojure.core/drop-last
([s] [n s])
  Return a lazy sequence of all but the last n (default 1) items in coll
nil
user=> (doc take-last)
-------------------------
clojure.core/take-last
([n coll])
  Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec.
nil
user=> (doc flatten)
-------------------------
clojure.core/flatten
([x])
  Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil.
nil
user=> (doc concat)
-------------------------
clojure.core/concat
([] [x] [x y] [x y & zs])
  Returns a lazy seq representing the concatenation of the elements in the supplied colls.
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

