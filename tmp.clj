; #46
; Flipping out
; 
; Difficulty:	Medium
; Topics:	higher-order-functions
; Write a higher-order function which flips the order of the arguments of an input function.

(= 3 ((__ nth) 2 [1 2 3 4 5]))
(= true ((__ >) 7 8))
(= 4 ((__ quot) 2 8))
(= [1 2 3] ((__ take) [1 2 3 4 5] 3))


#_(
user=> (doc nth)
-------------------------
clojure.core/nth
([coll index] [coll index not-found])
  Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, Java arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences.
nil
user=> (doc >)
-------------------------
clojure.core/>
([x] [x y] [x y & more])
  Returns non-nil if nums are in monotonically decreasing order,
  otherwise false.
nil
user=> (doc quot)
-------------------------
clojure.core/quot
([num div])
  quot[ient] of dividing numerator by denominator.
nil
user=> (doc take)
-------------------------
clojure.core/take
([n coll])
  Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n.
nil
user=>
)
