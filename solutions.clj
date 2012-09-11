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

; 34: Write a function which creates a list of all integers in a given range.
; (= (__ 1 4) '(1 2 3))
; forbidden: range
(fn [s e]
  (take (- e s) (iterate inc s)))

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

; 40: Write a function which separates the items of a sequence by an arbitrary
; value.
; (= (__ 0 [1 2 3]) [1 0 2 0 3])
; forbidden: interpose
(fn [sep coll]
  (drop-last (mapcat vector coll (repeat sep))))

; 41: Write a function which drops every Nth item from a sequence.
; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])  
(fn [coll n]
  (flatten 
    (concat 
      (map #(drop-last %) (partition n coll)) 
      (take-last (rem (count coll) n) coll))))
; We partition the sequence, drop last one from each, then stitch them back
; take care the remaining elements too

; 42: Write a function which calculates factorials.
; (= (__ 5) 120)
(fn [n]
  (apply * (range 1 (inc n))))
; clojure arithmetic functions can take a variable number of arguments

; 43: Write a function which reverses the interleave process into n number of
; subsequences.
; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(fn [coll n]
  (apply map list (partition n coll)))
; exploit map function's ability to take a variable number of collections as
; arguments

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

; 50: Write a function which takes a sequence consisting of items with different
; types and splits them up into a set of homogeneous sub-sequences. The internal
; order of each sub-sequence should be maintained, but the sub-sequences
; themselves can be returned in any order (this is why 'set' is used in the
; test cases).
; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
#(vals (group-by type %))

; 53: Given a vector of integers, find the longest consecutive sub-sequence of
; increasing numbers.  If two sub-sequences have the same length, use the one
; that occurs first. An increasing sub-sequence must have a length of 2 or
; greater to qualify.
; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
; (= (__ [7 6 5 4]) [])
(fn [coll]
  (->> (partition 2 1 coll) 
    (partition-by #(- (second %) (first %))) 
    (filter #(= 1 (- (second (first %)) (ffirst %)))) 
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
; we first create a list of neighoring pairs, partition them by their pair
; differences, keep those with difference 1, finally return the longest one

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
