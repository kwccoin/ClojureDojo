; #44
; Rotate Sequence
; 
; Difficulty:	Medium
; Topics:	seqs
; 
; Write a function which can rotate a sequence in either direction.
; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
; (= (__ 1 '(:a :b :c)) '(:b :c :a))
; (= (__ -4 '(:a :b :c)) '(:c :a :b))

#_(
user=> (doc mod)
-------------------------
clojure.core/mod
([num div])
  Modulus of num and div. Truncates toward negative infinity.
nil
user=> (doc concat)
-------------------------
clojure.core/concat
([] [x] [x y] [x y & zs])
  Returns a lazy seq representing the concatenation of the elements in the supplied colls.
nil
user=> (doc split-at)
-------------------------
clojure.core/split-at
([n coll])
  Returns a vector of [(take n coll) (drop n coll)]
nil
user=> 
user=> (doc comp)
-------------------------
clojure.core/comp
([f] [f g] [f g h] [f1 f2 f3 & fs])
  Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc.
nil
user=> (doc cons)
-------------------------
clojure.core/cons
([x seq])
  Returns a new seq where x is the first element and seq is
    the rest.
nil
user=> 
)
