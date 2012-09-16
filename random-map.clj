

; Just a toy.
; create a map with a string as key and a random number as value.

; lol try: (filter #(re-find #"Case" %) (map #(.getName %) (seq (.getMethods String))))

;(defn n-rand [n] (take n (repeatedly rand)))
; (map #(.toLowerCase %) (keyz))

;every 2-combination of letters
;(for [x (keyz) y (keyz)] (map str x y)) 

;apply a prefix
;(doseq [x ":" y (keyz)] (println (str x y)))

(use '[clojure.contrib.math :only (round)])
(defn keyz "create the keyz" [] (map str (map char (range 65 91))))
(defn foo [x] 
  (loop [times (dec x)
         k (keyz)
         s (keyz)]
    (if (zero? times)
      s
      (recur (dec times) k (for [x (keyz) y s] (map str (str x) y)))
)))
(defn rand-map
    ([] (rand-map 10)) 
    ([size] (let [
      h size  
      length (inc (round (/ h 26)))
      v (take h (repeatedly rand))
      k (map str (map first (take h (foo length))))] 
      (zipmap k v))
))

(defn med [] (let [m (rand-map 111)] (/ (reduce + (vals m)) (count (vals m)) )))
(filter #(or (> 0.45 %1) (< 0.55 %1)) (repeatedly 100 med))

;;;;

(for [x (range) :while (< x 10) 
      y (range) :while (<= y x)]
  [x y])

=>(
[0 0]
[1 0] [1 1]
[2 0] [2 1] [2 2]
[3 0] [3 1] [3 2] [3 3]
[4 0] [4 1] [4 2] [4 3] [4 4]
[5 0] [5 1] [5 2] [5 3] [5 4] [5 5]
[6 0] [6 1] [6 2] [6 3] [6 4] [6 5] [6 6]
[7 0] [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7]
[8 0] [8 1] [8 2] [8 3] [8 4] [8 5] [8 6] [8 7] [8 8]
[9 0] [9 1] [9 2] [9 3] [9 4] [9 5] [9 6] [9 7] [9 8] [9 9])

; compose and create a function, based on its arguments
(defn fnth [n]
  (apply comp
    (cons first
      (take (dec n) (repeat rest)))))

; create some keywords...
(map (comp keyword #(.toLowerCase %) name) '(a B C))

#_(

cons
clojure.core

    (cons x seq)

Returns a new seq where x is the first element and seq is
the rest.


keyword
clojure.core

    (keyword name)
    (keyword ns name)

Returns a Keyword with the given namespace and name. Do not use :
in the keyword strings, it will be added automatically.


repeat
clojure.core

    (repeat x)
    (repeat n x)

Returns a lazy (infinite!, or length n if supplied) sequence of xs.


name
clojure.core

    (name x)

Returns the name String of a string, symbol or keyword.
)
(let [c (range 1 10) m {}] (assoc m (apply str "@" c) c))

user=> (doseq [[x y] (map list [1 2 3] [1 2 3])] 
         (prn (* x y)))
1
4
9
nil

;; where
user=> (map list [1 2 3] [1 2 3])
((1 1) (2 2) (3 3))

user=> (doseq [[[a b] [c d]] (map list {:1 1 :2 2} {:3 3 :4 4})] 
         (prn (* b d)))
3
8
nil

;; where
user=> (map list {:1 1 :2 2} {:3 3 :4 4})
(([:1 1] [:3 3]) ([:2 2] [:4 4]))

-------------------------
clojure.core/map
([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
  Returns a lazy sequence consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments.

i.e.

user=> (map str '("sleep" "run" "jump"))
("sleep" "run" "jump")
user=> (map str '("sleep" "run" "jump") '(" well" " fast" " high"))

(defn create_keys [] (map #(str "@" %) (range 0 10)))
user=> (map list '( 1 2 3) '(100 200 300))
((1 100) (2 200) (3 300))

user=> (let [r (range 0 10)] (map str (map #(str "@" %) r) r))
(map str '("sleep" "jump" "run"))
("@00" "@11" "@22" "@33" "@44" "@55" "@66" "@77" "@88" "@99")

(let [r (range 0 10)] (map #(str "@" %) r))
("@0" "@1" "@2" "@3" "@4" "@5" "@6" "@7" "@8" "@9")

user=> (defn kys [] (let [r (range 0 10)] (map #(str "@" %) r)))
#'user/kys
user=> (map list (kys) (range 0 10))
(("@0" 0) ("@1" 1) ("@2" 2) ("@3" 3) ("@4" 4) ("@5" 5) ("@6" 6) ("@7" 7) ("@8" 8) ("@9" 9))

-------------------------
clojure.core/doseq
([seq-exprs & body])
Macro
  Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by "for".  Does not retain
  the head of the sequence. Returns nil.

(defn kys [] (let [r (range 0 10)] (map #(str "@" %) r)))
(map list (kys) (range 0 10))
;=> (("@0" 0) ("@1" 1) ("@2" 2) ("@3" 3) ("@4" 4) ("@5" 5) ("@6" 6) ("@7" 7) ("@8" 8) ("@9" 9))

(doseq [[x y] (map list (kys) (range 0 10))] (println (str x ":" y)))
(doseq [[x y] (map list (kys) (range 0 10))] (assoc {} x y))

(let [m {}] (doseq [[x y] (map list (kys) (range 0 10))] (when [m] (println (str x ":" y " in " m)))))

user=> (let [m {}] (doseq [[x y] (map list (kys) (range 0 10)), m (assoc m x y)] (println m)))
[@0 0]
[@1 1]
[@2 2]
[@3 3]
[@4 4]
[@5 5]
[@6 6]
[@7 7]
[@8 8]
[@9 9]


(defn makekeymap [m] 
  (for [[x y] (map list (kys) (range 0 10))] 
    [assoc m x y]))

(assoc {} (first (flatten (map list (kys) (range 0 10)))) (second (flatten (map list (kys) (range 0 10)))))

;;;

(defn kys [] (let [r (range 0 10)] (map #(str "@" %) r)))

(let [m {}] (doseq [[x y] (map list (kys) (range 0 10))] (when [m]
(println (str x ":" y " in " m)))))

(assoc {} (first (flatten (map list (kys) (range 0 10)))) (second
(flatten (map list (kys) (range 0 10)))))

(zipmap (range 1 5) (range 11 16))

(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  {:added "1.0"
   :static true}
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (next ks)
               (next vs))
        map)))

-------------------------
clojure.core/seq
([coll])
Returns a seq on the collection. If the collection is
empty, returns nil. (seq nil) returns nil. seq also works on
Strings, native Java arrays (of reference types) and any objects
that implement Iterable.
nil

-------------------------
clojure.core/loop
(loop [bindings*] exprs*)
Special Form
Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. Acts as a recur target.

Please see http://clojure.org/special_forms#loop
nil

-------------------------
recur
(recur exprs*)
Special Form
Evaluates the exprs in order, then, in parallel, rebinds
the bindings of the recursion point to the values of the exprs.
Execution then jumps back to the recursion point, a loop or fn method.

Please see http://clojure.org/special_forms#recur
nil

-------------------------
clojure.core/zipmap
([keys vals])
Returns a map with the keys mapped to the corresponding vals.
nil

-------------------------
clojure.core/next
([coll])
Returns a seq of the items after the first. Calls seq on its
argument. If there are no more items, returns nil.
nil

-------------------------
clojure.core/rest
([coll])
Returns a possibly empty seq of the items after the first. Calls seq on its
argument.
nil

;;;;;;;

(defn zipInMap [keys vals]
  (loop [map {}
        keys (seq keys)
        vals (seq vals)]
        (if (and keys vals)
        (recur (assoc m (first keys) (first vals))
               (next keys)
               (next vals))
        map)))
;;;;

(defn keyz "create the keys" [] (map str (map char (range 65 91))))
(defn zipInMap [k v] 
  (loop [m {} 
         kk k 
         vv v] 
         (if (and kk vv) 
         (recur (assoc m (first kk) (first vv)) 
           (next kk) 
           (next vv)) 
         m)))
(zipInMap ["a" "b"] [1 2])

;;;;;;;;;;;;;;;;;;;;

-------------------------
clojure.core/<
([x] [x y] [x y & more])
  Returns non-nil if nums are in monotonically increasing order,
  otherwise false.

;given a point 'xy' and a 'size'
;neighbours return the vector of the
;cross coordinates surround the point
;and filtered by the existence.

(defn neighbours 
  ; deltas are the 'cross' coordinates to check
  ; xy are the coordinates of a point
  ; size, the distance to check
  ([size xy] (let [deltas [[-1 0][1 0][0 -1][0 1]]] (neighbours deltas size xy)))
  ([deltas size xy]
    (filter 
    ;filter needs a predicate, so I define one here:
      (fn [xys-to-check] (every? #(< -1 % size) xys-to-check)) 
      ;this predicate just check that every point in xys-to-check, that must be a collection, is inside the 1st quadrant. A predicate must return a Boolean value. It use the '<' function with the 3rd parameter that means the size of the quadrant
      (map #(map + % xy) deltas) 
      ;this map find the vector of cross coordinates and is checked by the predicate i.e. filtered.
)))

;;;;;;;;;;;;;

;;lol try: (filter #(re-find #"Case" %) (map #(.getName %) (seq (.getMethods String))))

(defn keyz "create the keyz" [] (map str (map char (range 65 91))))
(map #(.toLowerCase %) (keyz))
(defn how-many [n] (take n (repeatedly rand)))
(use '[clojure.contrib.math :only (round)])

(let [h 10 
      l (inc (round (/ h 26)))
      v (take h (repeatedly rand))
      k (take h (keyz))] 
      (zipmap k v)
)

(defn foo [x] 
  (loop [times x
         k (keyz)
         s '()]
    (if (zero? times)
      s
      (recur (dec times) k (map str (seq k) (seq k)))
)))

;every 2-combination of letters
(for [x (keyz) y (keyz)] (map str x y))
;apply a prefix
(doseq [x ":" y (keyz)] (println (str x y)))
(defn extend-keys [list] (doseq [x (keyz) y list] (str x y)))

;;yum!
(defn asd [list] (for [x list y (keyz)] (map str x y)))
(asd (asd (keyz)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defn f [l ls]
  (loop [li l args ls]
    (if (empty? args)   
      li   
      (recur  
        (conj li (first args))  
        (rest args)   
      )
    )
  )
)

;;user=> (f '(0) '(1 2 3))
;(3 2 1 0)
;user=> (f '[0] '(1 2 3))
;[0 1 2 3]

