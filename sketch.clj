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
