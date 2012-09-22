; #43
; Reverse Interleave
; Difficulty:	Medium
; Topics:	seqs
;
; Write a function which reverses the interleave process into x number of subsequences.
; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
; (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
; (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

#_(
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
