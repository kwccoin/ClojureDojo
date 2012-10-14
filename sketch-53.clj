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
