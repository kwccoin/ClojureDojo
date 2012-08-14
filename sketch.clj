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
