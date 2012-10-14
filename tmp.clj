; #54 Partition a Sequence
;  
; Difficulty:	Medium
; Topics:	seqs core-functions
; 
; Special Restrictions: partition, partition-all
; 
; Write a function which returns a sequence of lists
; of x items each.
; Lists of less than x items should not be returned.
; 
; (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
; (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
; 

;

#_(
; user=> (doc when)
; -------------------------
; clojure.core/when
; ([test & body])
; Macro
;   Evaluates test. If logical true, evaluates body in an implicit do.
; nil
; user=> (doc cons)
; -------------------------
; clojure.core/cons
; ([x seq])
;   Returns a new seq where x is the first element and seq is
;     the rest.
; nil
; user=> (doc take)
; -------------------------
; clojure.core/take
; ([n coll])
;   Returns a lazy sequence of the first n items in coll, or all items if
;   there are fewer than n.
; nil
; user=> (doc drop)
; -------------------------
; clojure.core/drop
; ([n coll])
;   Returns a lazy sequence of all but the first n items in coll.
; nil
; user=> 
)
