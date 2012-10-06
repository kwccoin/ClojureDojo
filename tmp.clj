; #50 Split by Type 
; Topics:	seqs
;
; Write a function which takes a sequence consisting of items
; with different types and splits them up into a set of
; homogeneous sub-sequences. The internal order of each
; sub-sequence should be maintained, but the sub-sequences
; themselves can be returned in any order (this is
; why 'set' is used in the test cases).
;
; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
; (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
; (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

#_(
user=> (doc vals)
-------------------------
clojure.core/vals
([map])
  Returns a sequence of the map's values.
nil
user=> (doc group-by)
-------------------------
clojure.core/group-by
([f coll])
  Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll.
nil
user=> (doc type)
-------------------------
clojure.core/type
([x])
  Returns the :type metadata of x, or its Class if none
nil
user=> 
)
