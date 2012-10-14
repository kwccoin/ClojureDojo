
;; 4clojure solutions

; Huahai Solutions
; 21: Write a function which returns the Nth element from a sequence.
; (= (__ '(4 5 6 7) 2) 6)
; forbidden: nth
(fn [coll n] 
  ((apply comp (cons first (repeat n rest))) coll))
; We first compose n rest functions to get progressively shorter lists till the
; desired element is the head, then take the head. A less fancy version just
; uses nthnext, but it feels like cheating:
(fn [coll n]
  (first (nthnext coll n)))
#( ( vec %) %2)

; 22: Write a function which returns the total number of elements in a sequence.
; (= (__ '(1 2 3 3 1)) 5)
; forbidden: count
#(reduce + (map (fn [x] 1) %))
; We just turn each element into 1 and then add them up
; Note that (fn [x] 1) can be replaced by (constantly 1)

; 23: Write a function which reverses a sequence.
; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
; forbidden: reverse
#(into () %)
; We exploit the property of the list, which alway add new element
; in front of the head. Also that the clojure sequences' equality
; evaluation is element based, so [1 2 3] equals to '(1 2 3)

; 26: Write a function which returns the first X fibonacci numbers.
; (= (__ 6) '(1 1 2 3 5 8))
(fn [x]
  (take x
    ((fn fib [a b]
        (cons a (lazy-seq (fib b (+ a b))))) 
      1 1))) 
; we first recursively construct a lazy sequence of infinite number of
; fibonacci numbers

; 27: Write a function which returns true if the given sequence is a palindrome.
; (true? (__ '(1 1 3 3 1 1)))
(fn [coll]
  (let [rc (reverse coll) n (count coll)]
    (every? identity 
      (map #(= (nth coll %) (nth rc %)) (range (/ (dec n) 2))))))
; we naively compare half of the pairs of elment e(i) and e(n-i-1)
#(= (seq %) (reverse %))

; 28: Write a function which flattens a sequence.
; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
; forbidden: flatten
(fn flt [coll]
  (let [l (first coll) r (next coll)]
    (concat 
      (if (sequential? l)
        (flt l)
        [l])
      (when (sequential? r)
        (flt r)))))
; we basically treat the nested collection as a tree and recursively walk the
; tree. Clojure's flatten use a tree-seq to walk the tree.

; 29: Write a function which takes a string and returns a new string containing
;     only the capital letters.
; (= (__ "HeLlO, WoRlD!") "HLOWRD")    
(fn [coll]
  (apply str (filter #(Character/isUpperCase %) coll)))
; note the use of apply here, as str takes a number of args instead
; of a character collection

; 30: Write a function which removes consecutive duplicates from a sequence.
;  (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(fn cmprs [coll]
  (when-let [[f & r] (seq coll)] 
    (if (= f (first r)) 
      (cmprs r) 
      (cons f (cmprs r)))))  
; Basically a variant of the filter function. Note the sequence is destructed
; into first element f and the rest r.

; 31: Write a function which packs consecutive duplicates into sub-lists.
; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(fn [coll]
  ((fn pack [res prev coll]
    (if-let [[f & r] (seq coll)] 
      (if (= f (first prev)) 
        (pack res (conj prev f) r) 
        (pack (conj res prev) [f] r))) 
     (conj res prev))
    [] [(first coll)] (rest coll)))  
; res is the final list, prev keeps the immediate previous sub-list.
; A much simpler version use partition-by:
#(partition-by identity %)

(fn pack-consecutive [collection-param]
; utility function for packing the equals elements
((fn packer [result previous collection-param]
  ;work on collection
  ;but only if collection has some elements
  (if-let [[head & args] (seq collection-param)]
    (if (= head (first previous))
      ; if head, i.e. the first element in the collection is
      ; equals to the first element of the sub-list in check
      ; then add the head in this sub-list (previous) and call packer forward
      ; with the rest of the collection, called args
      (packer result (conj previous head) args)
      ; else add the previous list in the result, 
      ; create a new previous list to work on and go forward with packer
      (packer (conj result previous) [head] args)
    )
    ;if the collection is empty then return the last conjunction
    (conj result previous)
  )
)

; the packer function start with this initial arguments:
; result: [] an empty collection
; previous: [(first collection-param)] the first element of the collection
; collection: (rest collection-param) the rest of the collection
[] [(first collection-param)] (rest collection-param)
))

; 32. Duplicate a Sequence
; 
; Difficulty: Easy
; Topics: seqs
;     
; Write a function which duplicates each element of a sequence.
; (= (__ [1 2 3]) '(1 1 2 2 3 3))
; (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
; Solutions:
(fn [coll]
  (apply concat (map #(repeat 2 %) coll)))
;austintaylor's solution:
#(interleave % %)
;maximental's solution:
mapcat #(list % %)
;nikelandjelo's solution:
(fn [v] (reduce #(conj %1 %2 %2) [] v))
;norman's solution:
(fn dupl [items]  
  (if (empty? items)
    ()
    (let [f (first items) r (rest items)]
      (concat [f f] (dupl r)))))
;;;  

(fn duper [sequ]
  (let [s (seq sequ)]
    (lazy-seq
      (loop [f (first s)
             r (rest s)
             result (list)]
        (println "f: "f" r: "r" result: "result)
        (if (empty? r)
          (reverse (concat (repeat 2 f) result))
          (recur
            (first r)
            (rest r)
            (concat (repeat 2 f) result)))))))

; 33: Write a function which replicates each element of a sequence n number of
; times.
; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
(fn [coll n]
  (apply concat (map #(repeat n %) coll)))
; or more succintly:
(fn [coll n]
  (mapcat #(repeat n %) coll))

#_(
-------------------------
clojure.core/map
([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
  Returns a lazy sequence consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments.
-------------------------
clojure.core/mapcat
([f & colls])
  Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection.
-------------------------
clojure.core/concat
([] [x] [x y] [x y & zs])
  Returns a lazy seq representing the concatenation of the elements in the supplied colls.
-------------------------
clojure.core/repeatedly
([f] [n f])
  Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it
-------------------------
-------------------------
clojure.core/repeat
([x] [n x])
  Returns a lazy (infinite!, or length n if supplied) sequence of xs.
)   
    
; 33. Replicate a Sequence
; 
; Difficulty: Easy
; Topics: seqs
; 
; Write a function which replicates each element 
; of a sequence a variable number of times.
; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
; (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
; ( = (__ [4 5 6] 1) '(4 5 6))
; (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
; (= (__ [44 33] 2) [44 44 33 33]) 
          
(fn replicator [collection times]
  (mapcat #(repeat times %) collection))
; norman's solution:
#(mapcat (partial repeat %2) %1)
; nikelandjelo's solution:
#(apply concat (map (partial repeat %2) %1))

; 34: Write a function which creates a list of all integers in a given range.
; (= (__ 1 4) '(1 2 3))
; forbidden: range
(fn [s e]
  (take (- e s) (iterate inc s)))

;34. Implement range
; 
; Difficulty: Easy
; Topics: seqs core-functions
; Special restrictions: range
;
; Write a function which creates a list of all integers in a given range.
; (= (__ 1 4) '(1 2 3))
; (= (__ -2 2) '(-2 -1 0 1))
; (= (__ 5 8) '(5 6 7))

(fn ranger [from to]
  (take (- to from) (iterate inc from)))

; norman's solution:
(fn [start end]
  (seq
   (loop [i start vals []]
     (if (= i end)
       vals
       (recur (inc i) (conj vals i))))))

(defn vrange2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

#_(
-------------------------
clojure.core/lazy-seq
([& body])
Macro
  Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls.
-------------------------
clojure.core/while 
([test & body]) 
Macro
  Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil
-------------------------
clojure.core/iterate
([f x])
  Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects
)


; 38: Write a function which takes a variable number of parameters and returns
; the maximum value.
; forbidden: max, max-key
; (= (__ 1 8 3 4) 8)
(fn [x & xs]
  (reduce #(if (< %1 %2) %2 %1) x xs))

; 39: Write a function which takes two sequences and returns the first item
; from each, then the second item from each, then the third, etc.
; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
; forbidden: interleave
#(mapcat vector %1 %2) 

; 39. Interleave Two Seqs
;
; Difficulty:     Easy
; Topics: seqs core-functions
; Write a function which takes two sequences and returns the first item
; from each, then the second item from each, then the third, etc.
; Special Restrictions: interleave
; (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
; (= (__ [1 2 3 4] [5]) [1 5])
; (= (__ [30 20] [25 15]) [30 25 20 15])

(
(fn interleaver [coll1 coll2]
  (lazy-seq
    (let [seq1 (seq coll1)
          seq2 (seq coll2)]
      (when (and seq1 seq2)
        (cons              ;outer-cons
              (first seq1) ;first arg for outer-cons
              (cons        ;second arg for outer-cons and inner-cons
                    (first seq2) ;first arg for inner-cons
                    (interleaver (rest seq1) (rest seq2)) ;second arg for inner-cons
              ))))))
[1 2 3] [:a :b :c])

; Solutions:
; austintaylor's solution:
(fn [a0 b0]
  (loop [a a0 b b0 result '()]
    (if (and (seq a) (seq b))
      (recur
        (rest a)
        (rest b)
        (conj result (first a) (first b)))
      (reverse result))))
; maximental's solution:
mapcat list
; nikelandjelo's solution:
(fn inter [a b]
            (if (or (empty? a) (empty? b))
                []
                (concat [(first a) (first b)] (inter (rest a) (rest b)))))
; norman's solution:
(fn smash [one two]
  (when (every? not-empty [one two])
    (concat [(first one) (first two)] (smash (rest one) (rest two)))))

#_(
-------------------------
clojure.core/cons
([x seq])
  Returns a new seq where x is the first element and seq is
    the rest.
-------------------------
clojure.core/when
([test & body])
Macro
  Evaluates test. If logical true, evaluates body in an implicit do.
-------------------------
clojure.core/lazy-seq
([& body])
Macro
  Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls.
-------------------------
clojure.core/interleave
([c1 c2] [c1 c2 & colls])
  Returns a lazy seq of the first item in each coll, then the second etc.

   ;; This example takes a list of keys and a separate list of values and
   ;; inserts them into a map.
   (apply assoc {}
      (interleave [:fruit :color :temp]
          ["grape" "red" "hot"]))

  ; {:temp "hot", :color "red", :fruit "grape"}

  ;; Simple example:
  (interleave [:a :b :c] [1 2 3])
  ; (:a 1 :b 2 :c 3)
)

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


; 40: Write a function which separates the items of a sequence by an arbitrary
; value.
; (= (__ 0 [1 2 3]) [1 0 2 0 3])
; forbidden: interpose
(fn [sep coll]
  (drop-last (mapcat vector coll (repeat sep))))

; 40. Interpose a Seq
;
; Difficulty: Easy
; Topics: seqs core-functions
; 
; Write a function which separates the items of a sequence by an arbitrary value.
; (= (__ 0 [1 2 3]) [1 0 2 0 3])
; (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
; (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
; 
; Special Restrictions
; interpose

#_(
-------------------------
interpose
clojure.core
    (interpose sep coll)
Returns a lazy seq of the elements of coll separated by sep 
    
  ;; The quintessential interpose example:
      user> (def my-strings ["one" "two" "three"])
         
      user> (interpose ", " my-strings)
      => ("one" ", " "two" ", " "three")
         
      user> (apply str (interpose ", " my-strings))
      => "one, two, three"
         
      ;; Might use clojure.string/join if the plan is to join
      (use '[clojure.string :only (join)])
      user> (join ", " my-strings)
      => "one, two, three"
)

; Source:
1 (defn interpose
2   "Returns a lazy seq of the elements of coll separated by sep"
3   {:added "1.0"
4    :static true}
5   [sep coll] (drop 1 (interleave (repeat sep) coll)))

; Solutions:

(fn interposer
  [sep coll] (drop 1 (interleave (repeat sep) coll)))

; nikelandjelo's solution:
(fn [v s] (butlast (reduce #(conj %1 %2 v) [] s)))


; 41: Write a function which drops every Nth item from a sequence.
; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])  
(fn [coll n]
  (flatten 
    (concat 
      (map #(drop-last %) (partition n coll)) 
      (take-last (rem (count coll) n) coll))))
; We partition the sequence, drop last one from each, then stitch them back
; take care the remaining elements too

; 41. Drop Every Nth Item
;
; Difficulty: Easy
; Topics: seqs
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
  Returns a lazy seq representing the concatenation of the elements in
the supplied colls.
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

user=> (doc split-at)
-------------------------
clojure.core/split-at
([n coll])
  Returns a vector of [(take n coll) (drop n coll)].
nil
user=>
)


(defn dropper [coll n]
  (let [ [h t] (split-at n coll)]
    (do (println h " - " t)
      (if (< n (count (seq t)))
        (flatten (cons (drop-last h)  (dropper (seq t) n)))
        (flatten (cons (drop-last h)  (seq t)))
      )
    )))


(= ( 
(fn dropper [coll n]
  (let [ [h t] (split-at n coll)]
    (do (println h " - " t)
      (if (< n (count (seq t)))
        (flatten (cons (drop-last h)  (dropper (seq t) n)))
        (flatten (cons (drop-last h)  (seq t)))
      )
    )))
[1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
;= true

(= ( 
(fn dropper [coll n]
  (let [ [h t] (split-at n coll)]
    (do (println h " - " t)
      (if (<= n (count (seq t)))
        (flatten (cons (drop-last h)  (dropper (seq t) n)))
        (flatten (cons (drop-last h)  (seq t)))
      )
    )))
[:a :b :c :d :e :f] 2) [:a :c :e])
;= true

;Solutions:
(fn dropper [coll n]
  (let [ [h t] (split-at n coll)]
    (do (println h " - " t)
      (if (<= n (count (seq t)))
        (flatten (cons (drop-last h)  (dropper (seq t) n)))
        (flatten (cons (drop-last h)  (seq t)))
      )
    )))
; austintaylor's solution:
(fn [s x]
  (keep-indexed
    (fn [i a] (when (> (mod (inc i) x) 0) a))
    s))
; maximental's solution:
#(apply concat (partition-all (- %2 1) %2 %))
; nikelandjelo's solution:
(fn [s v] (remove nil? (map-indexed #(if (zero? (rem (inc %1) v)) nil %2) s)))
; norman's solution:
#(mapcat identity (partition-all (- %2 1) %2 %1))

#_(
user=> (doc identity)
-------------------------
clojure.core/identity
([x])
  Returns its argument.
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
user=> (doc partition-all)
-------------------------
clojure.core/partition-all
([n coll] [n step coll])
  Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end.
nil
user=> (doc keep-indexed)
-------------------------
clojure.core/keep-indexed
([f coll])
  Returns a lazy sequence of the non-nil results of (f index item). Note,
  this means false return values will be included.  f must be free of
  side-effects.
nil
user=> (doc map-indexed)
-------------------------
clojure.core/map-indexed
([f coll])
  Returns a lazy sequence consisting of the result of applying f to 0
  and the first item of coll, followed by applying f to 1 and the second
  item in coll, etc, until coll is exhausted. Thus function f should
  accept 2 arguments, index and item.
nil
user=> 
user=> (keep-indexed #(if (pos? %2) %1) [-9 0 29 -7 45 3 -8])
(2 4 5)
;; f takes 2 args: 'index' and 'value' where index is 0-based
;; when f returns nil the index is not included in final result
user=> (keep-indexed (fn [idx v]
                       (if (pos? v) idx)) [-9 0 29 -7 45 3 -8])
(2 4 5)
)

; ----

; 42: Write a function which calculates factorials.
; (= (__ 5) 120)
(fn [n]
  (apply * (range 1 (inc n))))
; clojure arithmetic functions can take a variable number of arguments

; #42
; Factorial Fun
; 
; Difficulty:	Easy
; Topics:	math
; 
; Write a function which calculates factorials.
; (= (__ 1) 1)
; (= (__ 3) 6)
; (= (__ 5) 120)
; (= (__ 8) 40320)
;
; The factorial function is formally defined by
;    n!=\prod_{k=1}^n k \!
; or recursively defined by
;    n! = \begin{cases} 
;           1 & \text{if } n = 0, 
;           \\ (n-1)!\times n &
;             \text{if } n > 0. 
;           \end{cases}
; The recurrence relation (n + 1)! = n! × (n + 1), valid for n > 0, extends to n = 0.

(fn my-fact [n]
  (if (zero? n)
    1
      (* (my-fact (dec n)) n)))

; austintaylor's solution:
(fn [x]
  (reduce * (range 1 (inc x))))
; maximental's solution:
#(apply * % (range 2 %))
; nikelandjelo's solution:
#(apply * (range 1 (inc %)))

#_(
user=> (doc *)
-------------------------
clojure.core/*
([] [x] [x y] [x y & more])
  Returns the product of nums. (*) returns 1.
nil
user=> (doc range)
-------------------------
clojure.core/range
([] [end] [start end] [start end step])
  Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end
  to infinity.
nil
user=> (doc apply)
-------------------------
clojure.core/apply
([f args* argseq])
  Applies fn f to the argument list formed by prepending args to argseq.
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
user=> 
)

; 43: Write a function which reverses the interleave process into n number of
; subsequences.
; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(fn [coll n]
  (apply map list (partition n coll)))
; exploit map function's ability to take a variable number of collections as
; arguments

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

; This solution comes from the combination of two
; function: apply and map.
; apply because if you were to try
;user=> (max [1 2 3])
; [1 2 3]
; You would get '[1 2 3]' for the result. In this case, 'max' has
; received one vector argument, and the largest of its arguments
; is that single vector.;; If you would like to find the largest
; item **within** the vector, you would need to use 'apply':
;user=> (apply max [1 2 3])
;3
; which is the same as (max 1 2 3)
; And map, because map Returns a lazy sequence consisting of the result
; of applying f to the
; set of first items of each coll, followed by applying f to the set
; of second items in each coll, until any one of the colls is
; exhausted. 

; Thus apply map will return a number of sublists equals to the
; param of partition, and the sublist will be composed of
; (/ (count coll) size):

user=> (partition 5 (range 10))
((0 1 2 3 4) (5 6 7 8 9))

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

(defn rotate [step coll]
  (let [[a b] (split-at (mod step (count coll)) coll)] (concat b a)))
(rotate  2 [1 2 3 4 5])
(rotate -2 [1 2 3 4 5])
(rotate  6 [1 2 3 4 5])
(rotate  1 '(:a :b :c))
(rotate -4 '(:a :b :c))

; Solutions:
(fn rotate [step coll]
  (let [[a b] (split-at (mod step (count coll)) coll)] (concat b a)))

; austintaylor's solution:
(fn [n s]
  (let [[a b] (split-at (mod n (count s)) s)]
    (concat b a)))

; maximental's solution:
(comp (partial apply concat)
  (juxt (partial apply drop)
     (partial apply take))
     (juxt (comp (partial apply mod)
       (juxt first
         (comp count second)))
           second)
      list)
(fn [n s] (#(concat (drop % s) (take % s)) (mod n (count s))))

;nikelandjelo's solution:
(fn rotate [n seq]
  (cond (zero? n)
    seq
      (neg? n)
      (recur (inc n) (concat [(last seq)] (butlast seq)))
      (pos? n)
      (recur (dec n) (concat (rest seq) [(first seq)]))))

; norman's solution:
(fn [n x]
  (let [howmany (mod n (count x))]
     (concat (drop howmany x) (take howmany x))))

#_(
; user=> (doc mod)
; -------------------------
; clojure.core/mod
; ([num div])
;   Modulus of num and div. Truncates toward negative infinity.
; nil
; user=> (doc concat)
; -------------------------
; clojure.core/concat
; ([] [x] [x y] [x y & zs])
;   Returns a lazy seq representing the concatenation of the elements in the supplied colls.
; nil
; user=> (doc split-at)
; -------------------------
; clojure.core/split-at
; ([n coll])
;   Returns a vector of [(take n coll) (drop n coll)]
; nil
; user=> 
; user=> (doc comp)
; -------------------------
; clojure.core/comp
; ([f] [f g] [f g h] [f1 f2 f3 & fs])
;   Takes a set of functions and returns a fn that is the composition
;   of those fns.  The returned fn takes a variable number of args,
;   applies the rightmost of fns to the args, the next
;   fn (right-to-left) to the result, etc.
; nil
; user=> (doc cons)
; -------------------------
; clojure.core/cons
; ([x seq])
;   Returns a new seq where x is the first element and seq is
;     the rest.
; nil
; user=> 
)

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

(defn flip-out-for-two [func]
  (fn [a b] (func b a)))
(defn flip-out [func]
  (fn [& args]
    (apply func (reverse args))))

; nikelandjelo's solution:
#(fn [a b] (% b a))
(fn flpout [f]
  (fn [& args]
    (apply f (concat (rest args) (take 1 args)))))

#_(
; user=> (doc nth)
; -------------------------
; clojure.core/nth
; ([coll index] [coll index not-found])
;   Returns the value at the index. get returns nil if index out of
;   bounds, nth throws an exception unless not-found is supplied.  nth
;   also works for strings, Java arrays, regex Matchers and Lists, and,
;   in O(n) time, for sequences.
; nil
; user=> (doc >)
; -------------------------
; clojure.core/>
; ([x] [x y] [x y & more])
;   Returns non-nil if nums are in monotonically decreasing order,
;   otherwise false.
; nil
; user=> (doc quot)
; -------------------------
; clojure.core/quot
; ([num div])
;   quot[ient] of dividing numerator by denominator.
; nil
; user=> (doc take)
; -------------------------
; clojure.core/take
; ([n coll])
;   Returns a lazy sequence of the first n items in coll, or all items if
;   there are fewer than n.
; nil
; user=>
)

; #47
; Contain Yourself

; The contains? function checks if a KEY is present in a given
; collection. This often leads beginner clojurians to use it 
; incorrectly with numerically indexed collections 
; like vectors and lists.


(contains? #{4 5 6} __)
(contains? [1 1 1 1 1] __)
(contains? {4 :a 2 :b} __)
(not (contains? '(1 2 4) __))

; 4
(reduce + (take 4 (repeatedly #(inc 0))))

#_(
-------------------------
clojure.core/contains?
([coll key])
  Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and Java arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'.
nil
user=> 
)

; #49
; Split a sequence
; 
; Topics:	seqs core-functions
;
; Write a function which will split a sequence into two parts.
;
; (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
; (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
; (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])
; Special Restrictions
; split-at

; user=> (doc split-at)
; -------------------------
; clojure.core/split-at
; ([n coll])
;   Returns a vector of [(take n coll) (drop n coll)]
; nil
; user=> (split-at 2 [1 2 3 4 5])
; [(1 2) (3 4 5)]

; Source:
(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  {:added "1.0"
   :static true}
  [n coll]
    [(take n coll) (drop n coll)])

#_(
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

; Solutions:
#(vals (group-by type %))
; austintaylor's solution:
(fn [s]
  (vals (group-by type s)))
; norman's solution:
(fn split [n]
  (set (vals (loop [items n tmpmap {}]
    (if (seq items)
      (recur (rest items)
        (merge-with concat
          tmpmap
          {(type (first items)) [(first items)]}))
      tmpmap)))))

#_(
; user=> (doc vals)
; -------------------------
; clojure.core/vals
; ([map])
;   Returns a sequence of the map's values.
; nil
; user=> (doc group-by)
; -------------------------
; clojure.core/group-by
; ([f coll])
;   Returns a map of the elements of coll keyed by the result of
;   f on each element. The value at each key will be a vector of the
;   corresponding elements, in the order they appeared in coll.
; nil
; user=> (doc type)
; -------------------------
; clojure.core/type
; ([x])
;   Returns the :type metadata of x, or its Class if none
; nil
; user=> 
)

; 50: Write a function which takes a sequence consisting of items with different
; types and splits them up into a set of homogeneous sub-sequences. The internal
; order of each sub-sequence should be maintained, but the sub-sequences
; themselves can be returned in any order (this is why 'set' is used in the
; test cases).
; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
#(vals (group-by type %))

; #53
; Longest Increasing Sub-Seq
;  
; Difficulty:	Hard
; Topics:	seqs
; 
; 53: Given a vector of integers, find the longest consecutive
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

;; hyone's solution to Longest Increasing Sub-Seq
; This function takes a collection and return the longest
; increasing sequence contained in it.
; Repetitions are not allowed but 'holes' yes.
(defn longest-inc-seq [coll]
  ; The core is reduce with this signature:
  ; (reduce [f val coll])
  ; The function f given to reduce is a two parameters function
  ; that simply return the longest (> 1) sequence of two.
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    ; the initial value for reduce, and the %1 for f,
    ; the %2 will the the first item in coll.
    ; Then f will be applied to that result and the
    ; second item in coll, etc.
    []
    ; The reduction will provide the coll (a lazy seq) for
    ; the reduce function above.
    ; We consider reductions with this signature:
    ; (reductions [f init coll])
    (reductions
      ; Reductions return a lazy seq of the intermediate values
      ; of the reduction of coll (the initial argument of
      ; our function longest-inc-seq) by a function f, and
      ; starting with init.
      ; The function f takes two arguments and check
      ; if second argument is greater than the last element of
      ; the first argument (thath is a collection).
      ; If it is so, conjoin this element in the first, else
      ; return a new collection with just this element.
      ; The function of the reduce above will check if this
      ; new collection have the best count to be the candidate
      ; for the longest sequence.
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      ; This is the initial parameter for reductions
      [(first coll)]
      ; This is the collection to check for the longest sequence.
      (rest coll))))
;
(defn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1) 
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []  
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))

; Solutions:
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

; austintaylor's solution:
(fn [s]
  (let [
    subseqs (filter
      #(not= 1 (count %))
      (mapcat
        (partial reductions conj [])
        (tree-seq
          (complement empty?)
          (comp list rest) s)))
      inc? (fn [s] (or (empty? s) (= s (range (first s) (inc (last s))))))]
  (last (sort-by count (filter inc? subseqs)))))

; maximental's solution:
(fn [C [s & z]]
  ((fn g [a [h & t]]
    (if h
      (if (< (last a) h)
        (g (conj a h) t)
        (let [q (g [h] t)]
          (if (next a)
            (if (< (C a) (C q)) q a)         
            q)))
      []))
   [s] `(~@z 0)))
count

; nikelandjelo's solution:
(fn [s]
        (->> (for [st (range (count s)) e (range st (inc (count s)))] (drop st (take e s)))
             (remove #(< (count %) 2))
             (filter #(apply < %))
             (reverse)
             (cons [])
             (apply max-key count)))

; norman's solution:
(fn lseq [in-vals]
  (letfn
      [(better-seq [seq1 seq2]
         (if (> (count seq2) (count seq1)) seq2 seq1))
       (lseq2 [vals]
         (loop [current-max []
                current-seq []
                items vals]
           (if-not (seq items)
             (better-seq current-max current-seq)
             (if (every? #(> (first items) %) current-seq)
               (recur current-max (conj current-seq (first items)) (rest items))
               (recur (better-seq current-max current-seq) [(first items)] (rest items))))))]
    (let [result (lseq2 in-vals)]
      (if (> (count result) 1)
        result
        []))))

; #53 - Appendix
; is-sequential? predicate function

; is-seq from 1
; Fast:
(defn iseq? [numbers]
  (loop [i 1
         s (seq numbers)]
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
      ; makes a new coll from numbers
      ; check if in this coll there is the preceding value
      (every? identity (map #(get (into [] numbers) (dec %) nil) numbers))))

; is-seq from 1
; Simple:
(defn iseq?3 [numbers]
  (= numbers (range 1 (inc (count numbers)))))

; is-seq from 1
(defn f [xs] 
  (every? 
    #(apply = %) 
    (map vector xs (iterate inc 1))))

; is-seq from 1
(defn f [xs] 
  (every? 
    true? 
    (map = xs (iterate inc 1))))

; from dnolen
; This function conj-if-sequential return a
; vector. This vector contains the fist element
; of the vector passed as argument and the
; others elements of the argument that are
; sequential to this first.
(defn conj-if-sequential
  ([] [])
  ([a] a)
  ; When this function receive 2 arguments
  ; a and b, makes a ckeck.
  ; This check need that the first argument (a)
  ; is a vector, if not it makes one.
  ; Then check if the last element of this vector
  ; is the preceding of the second argument
  ; of the function (b).
  ; If it so, it conj the second argument (b)
  ; in the vector made from the first argument (a)
  ([a b] (let [a (if (vector? a) a [a])]
           (if (= (inc (last a)) b)
             (conj a b)
             a))))

; Examples:
; user=> (reduce conj-if-sequential [2 3 4 6 8 1])
; [2 3 4]
; user=> (reduce conj-if-sequential [0 2 3 4 6 8 1])
; [0 1]
; user=> (reduce conj-if-sequential [6 0 2 3 4 6 8 1])
; [6]
; user=> (reduce conj-if-sequential [5 0 2 3 4 6 8 1])
; [5 6]
; user=> (reduce conj-if-sequential [9 0 2 3 4 6 8 1])
; [9]

; More general version
; This function works with nested associative structures
; and return a vector.
(defn sequential-seqs
  ([] [])
  ([a] a)
  ; When works with two arguments, create a variable n.
  ; n contains the last element of the last nested vector
  ; contained in a.
  ([a b] (let [n (last (last a))]
           ; If the second parameter (b) is the successive 
           ; of the new n...
           (if (and n (= (inc n) b))
             ; then update-in the nested structure a 
             ; the last element, conjoining the element b.
             ; The key for the last element in the nested a
             ; is simply the size of the structure a, minus one.
             (update-in a [(dec (count a))] conj b)
             ; Else return the nested structure a with a 
             ; new element conjoined, the vector with the
             ; element b (that is not the consecutive of n.
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
; user=> (doc =)
; -------------------------
; clojure.core/=
; ([x] [x y] [x y & more])
;   Equality. Returns true if x equals y, false if not. Same as
;   Java x.equals(y) except it also works for nil, and compares
;   numbers and collections in a type-independent manner.  Clojure's immutable data
;   structures define equals() (and thus =) as a value, not an identity,
;   comparison.
; nil
; user=> (doc every?)
; -------------------------
; clojure.core/every?
; ([pred coll])
;   Returns true if (pred x) is logical true for every x in coll, else
;   false.
; nil
; user=> 
; user=> (doc conj)
; -------------------------
; clojure.core/conj
; ([coll x] [coll x & xs])
;   conj[oin]. Returns a new collection with the xs
;     'added'. (conj nil item) returns (item).  The 'addition' may
;     happen at different 'places' depending on the concrete type.
; nil
; user=> 
; user=> (doc update-in)
; -------------------------
; clojure.core/update-in
; ([m [k & ks] f & args])
;   'Updates' a value in a nested associative structure, where ks is a
;   sequence of keys and f is a function that will take the old value
;   and any supplied args and return the new value, and returns a new
;   nested structure.  If any levels do not exist, hash-maps will be
;   created.
; nil
; user=> 
; user=> (def users [{:name "James" :age 26}  {:name "John" :age 43}])
; #'user/users
; user=> (update-in users [1 :age] inc)
; [{:name "James", :age 26} {:name "John", :age 44}]
; (defn char-cnt [s]
;   "Counts occurence of each character in s"
;   (reduce
;     (fn [m k]
;       (update-in m [k] (fnil inc 0)))
;   {}
;   (seq s)))
; ; Note use of fnil above - returns 0 if nil is passed to inc
; ; (avoids null pointer exception)
)

; 54: Write a function which returns a sequence of lists of x items each.
; Lists of less than x items should not be returned.
; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
; forbidden: partition, partition-all
(fn partition2 [n coll]
  (when (<= n (count coll))
    (cons (take n coll) (partition2 n (drop n coll)))))
; we recursively take n items till not enough items

; 55: Write a function that returns a map containing the number of occurences
; of each distinct item in a sequence.
; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
; forbidden: frequencies
(fn [coll]
  (let [gp (group-by identity coll)] 
    (zipmap (keys gp) (map #(count (second %)) gp))))
; note a map entry is just a two item vector, first item is the key, the
; second item is the value

; 56: Write a function which removes the duplicates from a sequence. Order of
; the items must be maintained.
; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
; forbidden: distinct
(fn [coll] 
  ((fn step [[x & xs] seen] 
     (when x
       (if (seen x) 
         (step xs seen)
         (cons x (step xs (conj seen x)))))) 
   coll #{}))
; we recursively go through the sequence, use a set to keep track of items
; we've seen, only return those we have not seen before.
  

; 58: Write a function which allows you to create function compositions. The
; parameter list should take a variable number of functions, and create a
; function applies them from right-to-left.
; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
; forbidden: comp
(fn [x & xs]
  (fn [& args]
    ((fn step [[f & fs] a]
       (if fs
         (f (step fs a))
         (apply f a)))
     (cons x xs) args)))
; step function takes the function list and the arguments, recursively builds
; an ever deeper call stack till at the end of the list, where the right most
; function is called with the given arguments.
    

; 59: Take a set of functions and return a new function that takes a variable
; number of arguments and returns sequence containing the result of applying
; each function left-to-right to the argument list.
; (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
; forbidden: juxt
(fn [x & xs]
  (fn [& args]
    (map #(apply % args) (cons x xs))))

; 60: Write a function which behaves like reduce, but returns each
; intermediate value of the reduction. Your function must accept either two
; or three arguments, and the return sequence must be lazy.
; (= (take 5 (__ + (range))) [0 1 3 6 10])
; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
; forbidden: reductions
(fn reductions2
  ([f init [x & xs]] 
   (cons init (lazy-seq (when x (reductions2 f (f init x) xs))))) 
  ([f coll] 
   (reductions2 f (first coll) (rest coll))))

; 61: Write a function which takes a vector of keys and a vector of values
; and constructs a map from them.
; (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
; forbidden: zipmap
#(into {} (map vector %1 %2))

; 62. Given a side-effect free function f and an initial value x
; write a function which returns an infinite lazy sequence of x,
; (f x), (f (f x)), (f (f (f x))), etc.  
; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
; forbidden: iterate
(fn iterate2 [f x]
  (cons x (lazy-seq (iterate2 f (f x)))))
; it turns out that clojure's own implmentation is the same

; 63. Given a function f and a sequence s, write a function which returns a
; map. The keys should be the values of f applied to each item in s. The value
; at each key should be a vector of corresponding items in the order they
; appear in s.
; (= (__ #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})
; forbidden group-by
(fn [f s]
  ((fn step [ret f [x & xs]]
     (if x
       (let [k (f x)]
         (step (assoc ret k (conj (get ret k []) x)) f xs))
       ret))
    {} f (seq s)))
; the get function takes a default argument for when the key is not found,
; which is used to initialize a vector here. Note the use of seq for s, as
; the collection may be a set, where the [x & xs] destructering doesn't work.
; Intead of recursively going over a sequence, we can also use reduce:
(fn [f s]
  (reduce 
    (fn [ret x]
      (let [k (f x)]
        (assoc ret k (conj (get ret k []) x))))
    {} s))

; 65: Write a function which takes a collection and returns one of :map, :set,
; :list, or :vector - describing the type of collection it was given.
; (= :map (__ {:a 1, :b 2}))
; forbidden: class, type, Class, vector?, sequential?, list?, seq?, map?, set?
; instance? getClass
(fn [coll]
  (let [x (rand-int 100) y (rand-int 100) 
        p [x y] c (conj coll z)]
    (cond 
      (= y (get c x)) :map
      (= p (get c p)) :set
      (= x (last (conj c x))) :vector
      :else :list)))
; we conj a random two element vector into the collection, map will treat it
; as a new key value pair, others treat it as a single item; set is a map too,
; so we can get the vector back with itself as the key; vector and list are
; differentiated by the position of the conj.

; 67: Write a function which returns the first x number of prime numbers.
(fn [x]
  (take x
        (remove 
          (fn [n] 
            (some #(= 0 (mod n %)) (range 2 (inc (int (Math/sqrt n))))))
          (iterate inc 2))))
; we just test each number n, each divided by numbers from 2 up to sqrt(n)

; 69: Write a function which takes a function f and a variable number of maps.
; Your function should return a map that consists of the rest of the maps
; conj-ed onto the first. If a key occurs in more than one map, the mapping(s)
; from the latter (left-to-right) should be combined with the mapping in the
; result by calling (f val-in-result val-in-latter)
; (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15}) {1 7, 2 10, 3 15})
; forbidden: merge-with
(fn [f m & ms]
  (reduce 
    (fn [ret x]
      (reduce 
        (fn [r k] 
          (conj r (if (r k) [k (f (r k) (x k))] (find x k)))) 
        ret (keys x))) 
    (cons m ms)))
; note a map is a function itself, so (r k) and (x k) works

; 70: Write a function which splits a sentence up into a sorted list of words.
; Capitalization should not affect sort order and punctuation should be ignored
; (= (__  "Have a nice day.") ["a" "day" "Have" "nice"])
(fn [s]
  (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))

; 73: A tic-tac-toe board is represented by a two dimensional vector. X is
; represented by :x, O is represented by :o, and empty is represented by :e. A
; player wins by placing three Xs or three Os in a horizontal, vertical, or
; diagonal row. Write a function which analyzes a tic-tac-toe board and returns
; :x if X has won, :o if O has won, and nil if neither player has won.
; (= nil (__ [[:e :e :e]
            ;[:e :e :e]
            ;[:e :e :e]]))
;(= :x (__ [[:x :e :o]
           ;[:x :e :e]
           ;[:x :e :o]]))
(fn [board]
  (let [i [0 1 2]
        c (take 12 (cycle i))
        p (flatten (map #(repeat 3 %) i))
        zip #(map vector %1 %2)
        win? (fn [w] 
               (some 
                 (fn [x] (every? #(= w (get-in board %)) x)) 
                 (partition 
                   3 (into (zip (into i p) c) (zip c (into (reverse i) p))))))]
    (cond 
      (win? :x) :x
      (win? :o) :o)))
; we basically enumerate all possible winning positions, which fall into
; some regular patterns. I am sure there are better ways, but in the
; interest of time... Note the use of get-in to fetech value in a multiple
; dimensional vector: (get-in board [x y])

; 74: Given a string of comma separated integers, write a function which
; returns a new comma separated string that only contains the numbers
; which are perfect squares.
; (= (__ "4,5,6,7,8,9") "4,9")
(fn [s]
  (->> (re-seq #"\d+" s)
    (map #(Integer/parseInt %))
    (filter (fn [x]
              (let [r (int (Math/sqrt x))]
                (= x (* r r)))))
    (interpose ",")
    (apply str)))

; 75: Two numbers are coprime if their greatest common divisor equals 1.
; Euler's totient function f(x) is defined as the number of positive integers
; less than x which are coprime to x. The special case f(1) equals 1. Write a
; function which calculates Euler's totient function.
; (= (__ 10) (count '(1 3 7 9)) 4)
(fn [n]
  (->> (range 2 n)
    (filter (fn [x]
              (= 1 ((fn gcd [a b]
                      (if (= 0 b) a (gcd b (mod a b))))
                    x n))))
    count
    inc))

; 77: Write a function which finds all the anagrams in a vector of words. A
; word x is an anagram of word y if all the letters in x can be rearranged in
; a different order to form y. Your function should return a set of sets, where
; each sub-set is a group of words which are anagrams of each other. Each
; sub-set should have at least two words. Words without any anagrams should not
; be included in the result.
; (= (__ ["meat" "mat" "team" "mate" "eat"]) #{#{"meat" "team" "mate"}})
; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
(fn [coll]
  (->> (group-by frequencies coll)
    (vals)
    (filter #(> (count %) 1))
    (map set )
    set))
; anagrams have the same distribution of characters

; 79: Write a function which calculates the sum of the minimal path through a
; triangle. The triangle is represented as a vector of vectors. The path should
; start at the top of the triangle and move to an adjacent number on the next
; row until the bottom of the triangle is reached.
; (= (__ [  [1]
          ;[2 4]
         ;[5 1 4]
        ;[2 3 4 5]])
   ;(+ 1 2 1 3)
   ;7)
(fn [triangle] 
  (apply min ((fn path-sum [p] 
                (concat 
                  (if (= (count triangle) (count p)) 
                    [(reduce + (map-indexed #(get-in triangle [%1 %2]) p))] 
                    (let [x (last p)] 
                      (concat 
                        (path-sum (conj p x)) 
                        (path-sum (conj p (inc x)))))))) 
              [0])))
; We enumerate all possible paths. The next step in a path can only go to the
; same or the plus one row index as the previous step, so the paths form a
; binary tree. We walk the tree recursively, building a row index vector p for
; each path.

; 81: Reimplement set intersection
#(set (filter %1 %2))
; sets are functions too, so this works

; 82: A word chain consists of a set of words ordered so that each word differs
; by only one letter from the words directly before and after it. The one
; letter difference can be either an insertion, a deletion, or a substitution.
; Here is an example word chain:
; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
; Write a function which takes a sequence of words, and returns true if they
; can be arranged into one continous word chain, and false if they cannot.
; (= false (__ #{"cot" "hot" "bat" "fat"}))
; (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
(fn [word-set]
  (letfn [(edit-dist [a b] 
            (cond 
              (not (or a b)) 0 
              (not b) (count a) 
              (not a) (count b) 
              :else (let [ra (next a) rb (next b)] 
                      (if (= (first a) (first b)) 
                        (edit-dist ra rb) 
                        (+ 1 (min 
                               (edit-dist ra rb) 
                               (edit-dist ra b) 
                               (edit-dist a rb)))))))
          (find-paths [graph start seen] 
            (if (seen start) 
              seen
              (for [n (graph start)] 
                (find-paths graph n (conj seen start)))))] 
    (let [graph (into {} 
                      (for [s word-set] 
                        [s (filter #(= 1 (edit-dist s %)) word-set)]))]
      (if (some (fn [w] 
                  (some #(= word-set %) 
                        (flatten (find-paths graph w #{})))) 
                word-set) 
        true false))))
; This problem consists of two sub-problems: A. Determine the edit distance
; between two strings. For brevity, we just used the standard recursive
; algorithm instead of dynamic programming. B. For the graph of strings
; connected by edges of edit distance 1, find a simple (no loop) path that
; goes through all strings once and only once. The graph is represented as
; a map of adjacent node lists. We enumerate all simple paths in the graph
; until we found one going through all nodes.

; 84: Write a function which generates the transitive closure of a binary
; relation. The relation will be represented as a set of 2 item vectors.
; (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
;   (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
; (let [progeny
;       #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
;         (= (__ progeny)
;              #{["father" "son"] ["father" "grandson"]
;                     ["uncle" "cousin"] ["son" "grandson"]}))
(fn [relation]
  (letfn [(expand [r] 
            (let [m (into {} r)] 
              (->> (concat 
                     r
                     (for [[k v] m] 
                       (when-let [nv (m v)] [k nv]))) 
                (filter identity) 
                set)))
          (first-consecutive [pred [f & rs]] 
            (when rs
              (if (pred f (first rs))
                f
                (recur pred rs))))]
    (first-consecutive = (iterate expand relation))))
; we iteratively expand the set of transitive relation, until the set no
; longer changes

; 85: Write a function which generates the power set of a given set. The power
; set of a set x is the set of all subsets of x, including the empty set and x
; itself.
; (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(fn [s]
  (reduce 
    (fn [init e] 
      (set (concat init (map #(conj % e) init) [#{e}])))
    #{#{}} s))
; we just add one element at a time

; 86: Happy numbers are positive integers that follow a particular formula: take
; each individual digit, square it, and then sum the squares to get a new number
; Repeat with the new number and eventually, you might get to a number whose
; squared sum is 1. This is a happy number. An unhappy number (or sad number) is
; one that loops endlessly. Write a function that determines if a number is
; happy or not.
; (= (__ 7) true)
; (= (__ 986543210) true)
(fn [x]
  (letfn [(digits [n]
            (for [y (iterate (partial * 10) 1) :while (<= y n)]
              (rem (int (/ n y)) 10)))
          (sqr-sum [ds]
            (reduce + (map #(* % %) ds)))]
    (let [r (some #{1 4} (iterate (comp sqr-sum digits) x))]
      (cond
        (= 1 r) true
        (= 4 r) false))))
; it turns out that 4 is a sad number, as it results into an infinite loop

; 88: Write a function which returns the symmetric difference of two sets. The
; symmetric difference is the set of items belonging to one but not both of
; the two sets.
; (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
#(set (remove (set (filter %1 %2)) (into %1 %2)))
; we remove the intersection from the union

; 89: Starting with a graph you must write a function that returns true if it
; is possible to make a tour of the graph in which every edge is visited exactly
; once.  The graph is represented by a vector of tuples, where each tuple
; represents a single edge.  The rules are:
; - You can start at any node.  
; - You must visit each edge exactly once.  
; - All edges are undirected.
; (= true (__ [[1 2] [2 3] [3 4] [4 1]]))
; (= false (__ [[1 2] [2 3] [2 4] [2 5]]))
; (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]]))
; (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e] [:b :e] [:a :d] [:b :d]
;              [:c :e] [:d :e] [:c :f] [:d :f]]))
(fn [edge-list]
  (let [graph (apply merge-with 
                #(into %1 %2) 
                (apply concat 
                  (map-indexed 
                    (fn [i [k v]] 
                      [{k #{{:node v :index i}}} 
                       {v #{{:node k :index i}}}]) 
                    edge-list)))]
    (if (some
          (fn [node] 
            (some 
              identity 
              (flatten 
                ((fn visit [n vs] 
                   (if (every? #(vs (:index %)) (graph n)) 
                     (if (every? identity vs) true false) 
                     (for [x (graph n)] 
                       (when-not (vs (:index x)) 
                         (visit (:node x) (assoc vs (:index x) true))))))
                 node (vec (repeat (count edge-list) false)))))) 
          (set (apply concat edge-list))) 
      true false)))
; This problem looks similar to problem 82 as both are graph traversals, but
; the graphs are quite different. Here redundent edges exist, so we cannot use
; a set or a map to track edge visits, we instead use a vector of booleans.
; Also, the condition is to traverse all edges instead of all nodes. We again
; use a map of adjacent node lists as the graph, but supplement each adjacent
; node with the index of the corresponding edge. Finally, here a node can be
; visited multiple times, and we terminates a path at a node only when all of
; its edges have already been visited.

; 91: Given a graph, determine whether the graph is connected. A connected
; graph is such that a path exists between any two given nodes.  
; -Your function must return true if the graph is connected and false otherwise.
; -You will be given a set of tuples representing the edges of a graph. Each
;  member of a tuple being a vertex/node in the graph.  
; -Each edge is undirected (can be traversed either direction).
;  (= false (__ #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]}))
;  (= true (__ #{[1 2] [2 3] [3 1][4 5] [5 6] [6 4] [3 4]}))
(fn [edge-list]
  (let [graph (apply merge-with 
                #(into %1 %2) 
                (apply concat 
                  (map (fn [[k v]] [{k #{v}} {v #{k}}]) edeg-list)))] 
    (if (some #(= (count %) (count graph)) 
              (flatten 
                ((fn paths [node seen] 
                   (if (seen node) 
                     seen
                     (for [x (graph node)] 
                       (paths x (conj seen node))))) 
                 (ffirst graph) #{}))) 
      true false)))
;  This graph traversal problem is simpler than both 82 and 89. We only  need
;  to start searching from any one of the nodes instead of all nodes. But the
;  pattern of the code is similar.

; 92: Write a function to parse a Roman-numeral string and return the number it
; represents. You can assume that the input will be well-formed, in upper-case,
; and follow the subtractive principle. You don't need to handle any numbers
; greater than MMMCMXCIX (3999), the largest number representable with
; ordinary letters.
; (= 827 (__ "DCCCXXVII"))
; (= 48 (__ "XLVIII"))
(fn [s]
  (let [snum {[\C \M] 900  [\C \D] 400 [\X \C] 90 
             [\X \L] 40 [\I \X] 9 [\I \V] 4}
        nums {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (letfn [(sum-snum [[f & r]]
                      (if f
                        (+ (if-let [n (snum [f (first r)])] 
                             n 0)
                           (sum-snum r))
                        0))
            (del-snum [[f & r]]
                         (when f
                           (if (snum [f (first r)])
                             (del-snum (rest r))
                             (cons f (del-snum r)))))]
      (reduce + (sum-snum s) (map nums (del-snum s))))))
; We first find and sum the special numbers (4, 9, etc), remove them and sum
; the rest.  

; 93: Write a function which flattens any nested combination of sequential
; things (lists, vectors, etc.), but maintains the lowest level sequential
; items. The result should be a sequence of sequences with only one level of
; nesting.
; (= (__ '((1 2)((3 4)((((5 6))))))) '((1 2)(3 4)(5 6)))
(fn pf [coll]
  (let [l (first coll) r (next coll)]
    (concat 
      (if (and (sequential? l) (not (sequential? (first l))))
        [l]
        (pf l))
      (when (sequential? r)
        (pf r)))))
; this is just a slight modification of the solution to problem 28.

; 94: The game of life is a cellular automaton devised by mathematician John
; Conway.  The 'board' consists of both live (#) and dead ( ) cells. Each cell
; interacts with its eight neighbours (horizontal, vertical, diagonal), and its
; next state is dependent on the following rules: 1) Any live cell with fewer
; than two live neighbours dies, as if caused by under-population.  2) Any live
; cell with two or three live neighbours lives on to the next generation.  3)
; Any live cell with more than three live neighbours dies, as if by overcrowding
; . 4) Any dead cell with exactly three live neighbours becomes a live cell, as
; if by reproduction.  Write a function that accepts a board, and returns a
; board representing the next generation of cells.
;(= (__ ["      "
        ;" ##   "
        ;" ##   "
        ;"   ## "
        ;"   ## "
        ;"      "])
   ;["      "
    ;" ##   "
    ;" #    "
    ;"    # "
    ;"   ## "
    ;"      "])
(fn [board]
  (let [offsets [[-1 -1] [-1 0] [-1 1]
                 [0 -1] [0 1]
                 [1 -1] [1 0] [1 1]]
        height (count board)
        width (count (first board))
        get-state (fn [[x y] [dx dy]]
                    (let [c (+ x dx) r (+ y dy)] 
                      (if (or (< c 0) (= c width) (< r 0) (= r height))
                        \space
                        (get-in board [r c]))))
        count-lives (fn [p]
                      (reduce + (map #(if (= \# (get-state p %)) 1 0) offsets)))
        next-state (fn [s p]
                     (let [n (count-lives p)] 
                       (if (or (= n 3)
                               (and (= s \#) (= n 2)))
                         \#
                         \space)))] 
    (->> (for [y (range height) x (range width)]
           (next-state (get-in board [y x]) [x y]))
      (partition width)
      (map #(apply str %))
      vec)))
; This is straight-forward. The only tricky part is to remember that the order
; of paramaters for the get-in function and the x-y coordinates is opposite to
; each other.  

; 95: Write a predicate which checks whether or not a given sequence represents
; a binary tree. Each node in the tree must have a value, a left child, and a
; right child.
; (= (__ '(:a (:b nil nil) nil)) true)
; (= (__ '(:a (:b nil nil))) false)
; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]]) true)
(fn bt? [t]
  (if (or (not (sequential? t))
          (and (= 3 (count t))
               (bt? (second t))
               (bt? (last t))))
    true false))
; I think one of the unit tests of the problem is wrong:
; (= (__ [1 [2 [3 [4 false nil] nil] nil] nil]) false)
; why shouldn't "false" be a legal tree node, or why should leaf have to be nil?

; 96: Let us define a binary tree as "symmetric" if the left half of the tree
; is the mirror image of the right half of the tree. Write a predicate to
; determine whether or not a given binary tree is symmetric.
; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
; (= (__ '(:a (:b nil nil) nil)) false)
; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          ;[2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]) true)
(fn [t]
  ((fn mir? [l r]
     (if (or (= nil l r)
             (and (= (first l) (first r))
                  (mir? (second l) (last r))
                  (mir? (last l) (second r))))
       true false)) 
   (second t) (last t)))

; 97: Pascal's triangle is a triangle of numbers computed using the following
; rules:
; - The first row is 1.
; - Each successive row is computed by adding together adjacent numbers in the
;   row above, and adding a 1 to the beginning and end of the row.  
; Write a function which returns the nth row of Pascal's Triangle.
; (= (map __ (range 1 6))
   ;[     [1]
        ;[1 1]
       ;[1 2 1]
      ;[1 3 3 1]
     ;[1 4 6 4 1]])
(fn [n]
  (nth (iterate 
         (fn [pre] 
           (vec 
             (concat 
               [1] 
               (map (fn [[f s]] (+ f s)) (partition 2 1 pre)) 
               [1])))
         [1])
       (dec n)))

;; Justin Hamilton
;; This only contains problems that require an actual function

;; imports
(use 'clojure.set)
(use 'clojure.contrib.math)

;; Double Down: Write a function which doubles a number.
(defn double-down [x]
  (* x 2))

;; Hello World: Write a function which returns a personalized greeting.
(defn hello-world [name]
  (str "Hello, " name "!"))

;; Last Element: Write a function which returns the last element in a sequence.
(defn last-element [x]
  (first (reverse x)))

;; Penultimate Element: Write a function which returns the second to last element from a sequence.
(defn pen-elem [x]
  (nth x (- (count x) 2)))

;; Nth element: Write a function which returns the Nth element from a sequence.
(defn nth-elem [s x]
  (last (take (inc x) s)))

;; Count a Sequence: Write a function which returns the total number of elements in a sequence.
(defn count-a-seq [lat]
  (reduce (fn [x y] (+ x 1)) 0 lat))

;; Sum It All Up: Write a function which returns the sum of a sequence of numbers.
(defn sum-it-al [lat]
  (reduce + lat))

;; Find the odd numbers: Write a function which returns only the odd numbers from a sequence.
(defn odd-numbers [lat]
  (filter odd? lat))

;; Reverse a Sequence: Write a function which reverses a sequence.
(defn rev-seq [lat]
  (into '() lat))

;; Palindrome Detector: Write a function which returns true if the given sequence is a palindrome.
(defn palindrome? [lat]
  (if (string? lat) (= lat (apply str (reverse lat)))
      (= lat (reverse lat))))

;; Fibonacci Sequence
(defn fib [n]
  (take n ((fn fib-recur [a b]) (cons a (lazy-seq (fib-recur b (+ a b)))) 1 1)))

;; Get the Caps: Write a function which takes a string and returns a new string containing only the capital letters.
(defn get-caps [s]
  (apply str (map char (filter #(and (<= 65 %) (<= % 90)) (map int n)))))

;; Maximum Value: Write a function which takes a variable number of parameters and returns the maximum value.
(defn max-val [x & xs]
  (reduce #(if (< %1 %2) %2 %1) (flatten (cons x xs))))

;; Implement range: Write a function which creates a list of all integers in a given range.
(defn new-range [a b]
  (take (- b a) (iterate inc a)))

;; Flatten a Sequence: Write a function which flattens a sequence.
(defn flat [n]
  (let [[x & xs] n]
    (cond
     (empty? n) '()
     (coll? x) (concat (flat x) (flat xs))
     :else (cons x (flat xs)))))

;; Duplicate a Sequence: Write a function which duplicates each element of a sequence.
(defn dup-seq [lat]
  (reduce concat (map #(take 2 (repeat %)) lat)))

;; Compress a Sequence
(defn comp-seq [n]
  (map first (partition-by identity n)))

;; Factorial Fun: Write a function which calculates factorials.
(defn factorial [n]
  (reduce * (range 1 (inc n))))

;; Replicate a Sequence: Write a function which replicates each element of a sequence a variable number of times.
(defn replicate [lat n]
  (reduce concat (map #(take n (repeat %)) lat)))

;; Interleave Two Seqs: Write a function that takes 2 seqs and returns the 1st item from both, 2nd, etc.
(defn interleave [a b]
  (letfn [(iter [a b]
            (let [[x & xs] a
                  [y & ys] b]
              (cons x (cons y (lazy-seq (iter xs ys))))))]
    (take (* 2 (min (count a) (count b))) (iter a b))))

;; Interpose a Seq: Write a function which separates the items of a sequence by an arbitrary value.
(defn inter [a lat]
  (let [[x & xs] lat]
    (if (empty? xs) (list x)
        (cons x (cons a (iter a xs))))))

;; Pack: Write a function which packs consecutive duplicates into sub-lists.
(defn pack [n]
  (partition-by identity n))

;; Drop Every Nth Item: Write a function which drops every Nth item from a sequence.
(defn drop-nth [lat n]
  (flatten (map #(if (= (count %) n) (drop-last %) %) (partition-all n lat))))

;; Flipping Out: Write a higher-order function which flips the order of the arguments of an input function.
(defn flip-out [f]
  (fn [& args] (apply f (reverse args))))

;; Split a Sequence: Write a function which will split a sequence into two parts.
(defn split-a-seq [n lat]
  (cons (take n lat) (cons (drop n lat) '())))

;; Rotate a Sequence: Write a function which can rotate a sequence in either direction.
(defn rotate-a-seq [n lat]
  (let [x (count lat)
        a (if (neg? n) (reverse (take (mod (* n -1) x) (reverse lat)))
              (drop (mod n x) lat))
        b (if (neg? n) (take (mod (+ x n) x) lat)
              (take (mod n x) lat))]
    (concat a b)))

;; Reverse Interleave: Write a function which reverses the interleave process into x number of subsequences.
(defn rev-inter [lat n]
  (partition (quote (count lat) n) (apply interleave (partition n lat))))

;; Count Occurences
(defn count-occurences [coll]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} col))

;; Map Construction: Write a function which takes vector of keys and values and constructs a map from them.
(defn map-con [a b]
  (apply hash-map (interleave a b)))

;; Greatest Common Divisor: Given 2 ints write a function which returns gcd
(defn gcd [a b]
  (if (= b 0) a
      (recur b (rem a b))))

;; Partition a Sequence: Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(defn partition-seq [n coll]
  (loop [coll coll
         held []]
    (if (or (empty? coll) (< (count coll) n)) held
        (recur (drop n coll) (conj held (take n coll))))))

;; Half Truth: Write a function which takes a variable num of bools. It should return true only if some of the params are true.
(defn half-truth [& more]
  (let [expr (and (some true? more) (some false? more))]
    (true? expr)))

;; Least Common Multiple: Write a function which calculates lcm.
(defn lcm [& xs]
  (letfn [(gcd [a b]
            (if (= b 0) a
                (recur b (rem a b))))]
    (reduce #(/ (* %1 %2) (gcd %1 %2)) more)))

;; Group a Sequence: Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s.
;;                   The value at each key should be a vector of corresponding items in the order they appear in s.
(defn group-seq [f col]
  (reduce #(assoc %1 (f %2) (conj (apply vector (%1 (f %2))) %2)) {} col))

;; Find Distinct Items: Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
(defn distinct-items [items]
  (reduce (fn [x y] (if (nil? (some (fn [z] (= y z)) x)) (conj x y)
                        x)) [] items))

;; Pascal's Triangle: Write a function which returns the nth row of Pascal's Triangle.
(defn pascal-row [row]
  (letfn [(fact [x]
            (apply * (range 1 (inc x))))
          (n-choose-k [n k]
            (/ (fact n) (* (fact k) (fact (- n k)))))]
    (map #(n-choose-k (dec row) %) (range row))))

;; To Tree, or not to Tree: Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child.
(defn is-binary? [tree]
  (cond
   (false? tree) false
   (not (coll? tree)) true
   (not= (count tree) 3) false
   :else (and (is-binary? (second tree)) (is-binary? (nth tree 2)))))

;; Juxtaposition: Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.
(defn new-juxt [& fs]
  (fn [& xs]
    (reduce #(conj %1 (apply %2 xs)) [] fs)))

;; Symmetric Difference: Write a function that returns the symmetric difference of two sets.
(defn sym-diff [a b]
  (let [dis (clojure.set/intersection a b)
        a-sub (clojure.set/difference a dis)
        b-sub (clojure.set/difference b dis)]
    (clojure.set/union a-sub b-sub)))

;; Product Digits: Write a function which multiples 2 numbers and returns the results as a sequence
(defn prod-dig [a b]
  (let [prod (* a b)
        len (count (str prod))]
    (map #(rem % 10) (reverse (take len (iterate #(quot % 10) prod))))))

;; Cartesian Product: Write a func that calcs the Cartesian product of 2 sets
(defn cartesian-product [x y]
  (set (for [a x b y] [a b])))

;; Set Intersection: Write a function that returns the intersection of 2 sets
(defn set-intersection [x y]
  (let [x-sub-y (clojure.set/difference x y)
        y-sub-x (clojure.set/difference y x)
        full-diff (clojure.set/union x-sub-y y-sub-x)
        full-set (clojure.set/union x y)]
    (clojure.set/difference full-set full-diff)))

;; Simple closures: Given an integer n return a function (f x) which computes x ^ n.
(defn simple-closure [x]
  (fn [y] (reduce * (take x (repeat y)))))

;; Read a binary number: Convert a binary number, provided in a string
(defn read-binary [bin]
  (letfn [(my exp [x y]
            (reduce * (repeat y x)))
          (to-num [idx ch]
            (if (= ch \1) (my-exp 2 idx)
                0))]            
    (reduce + (map-indexed to-num (reverse bin)))))

;; Re-implement map producing a lazy-seq
(defn re-map [f col]
  (if (seq col) (lazy-seq
               (cons (f (first col)) (re-map f (rest col))))
      nil))

;; Re-implement iterate
(defn new-iterate [f x]
  (lazy-seq
   (cons x (new-iterate f (f x)))))

;; Find if a map contains a key whose value is nil
(defn key-finder [k m]
  (and (not (nil? (some #(= k %) (keys m)))) 
       (nil? (k m))))

;; Infix calculator
(defn simple-calc [& args]
  (if (= (count args) 1) (first args)
      (let [[x op y & col] args]
        (recur (cons (op x y) col)))))

;; 67. Prime Numbers
(defn first-n-primes [n]
  (letfn [(is-prime [x]
            (letfn [(prime-iter [i num root]
                      (cond
                       (> i root) true
                       (= 0 (rem num i)) false
                       :else (recur (inc i) num root)))]
              (prime-iter 2 x (clojure.contrib.math/sqrt x))))
          (next-prime [x]
            (if (is-prime? (inc x)) (inc x)
                (recur (inc x))))]
    (take n (iterate next-prime 2))))

;; 143: Compute the cross-product of 2 3-d vectors
(defn cross-prod [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

;; 120: Sum of square of digits (www.4clojure.com/problem/120)
(defn sos-o-digits [num]
  (letfn [(num->digits [num]
            (letfn [(numiter [val col]
                      (if (>= 0 val) col
                          (recur (quot val 10)
                                 (conj (apply list col) (rem val 10)))))]
              (numiter num [])))                    
          (sum-of-square [digits]
            (reduce + (map #(* % %) digits)))
          (less-than-sos? [num]
              (let [digits (num->digits num)
                    sos (sum-of-square digits)]
                (< num sos)))]
    (count (filter less-than-sos? num))))

;; 74: Filter Perfect Squares
(defn filter-ps [x]
  (letfn [(string->int-seq [nums]
            (map read-string (clojure.string/split nums #",")))
          (perfect-square? [x]
            (if (= 0 (count (for [n (range (inc (Math/sqrt x)))
                                  :when (= (* n n) x)] x))) false
                true))]
  (apply str 
    (interpose "," 
      (map str 
           (filter perfect-square? (string->int-seq x)))))))


;; 147: Pascal's Trapezoid
(defn pascal-trapezoid [row]
  (letfn [(two-map [f col]
            (cond
              (or (empty? col) (empty? (rest col))) []
              :else (cons (f (first col) (second col))
                             (lazy-seq (two-map f (rest col))))))
          (next-row [row]
            (conj (vec (cons (first row) (two-map + row))) (last row)))]
    (cons row (lazy-seq (pascal-trapezoid (next-row row))))))


;; 58: Function Composition
(defn my-comp [f & r]
  (fn [& x]
    (letfn [(comp-maker [r]
              (cond
               (empty? r) x
               (empty? (rest r)) (apply (first r) x)
               :else ((first r) (comp-maker (rest r)))))]
      (f (comp-maker r)))))


;; 80: Perfect Numbers
(defn perfect-num? [n]
  (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n)))))
