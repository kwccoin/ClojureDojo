; penultimate

(defn penultimate2 "P02" [l]
(if (empty? (rest (rest l)))
(first l)
(penultimate2 (rest l))))

;;;;;;;;;;;;;;;;;;;;;;;;;

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

; or:
(fn [coll n] (first (drop n coll)))

#(loop [items %1 n %2]
    (if (= n 0) 
       (first items)
       (recur (rest items) (dec n))))

#(first (drop %2 %1))

; 22: Write a function which returns the total number of elements in a sequence.
; (= (__ '(1 2 3 3 1)) 5)
; forbidden: count
#(reduce + (map (fn [x] 1) %))
; We just turn each element into 1 and then add them up
; Note that (fn [x] 1) can be replaced by (constantly 1)

#(loop [items %1 n 0]  
  (if (empty? items) 
     n
     (recur (rest items) (inc n))))

#(apply + (map (fn [_] 1) %))

; 23: Write a function which reverses a sequence.
; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
; forbidden: reverse
#(into () %)
; We exploit the property of the list, which alway add new element
; in front of the head. Also that the clojure sequences' equality
; evaluation is element based, so [1 2 3] equals to '(1 2 3)

(fn [s]
  (loop [s s
         res []]
           (if (empty? s)
              res
              (recur (rest s) (cons (first s) res)))))


#_(loop
clojure.core

    (loop bindings & body)

Evaluates the exprs in a lexical context in which the symbols in
the binding-forms are bound to their respective init-exprs or parts
therein. Acts as a recur target.)

;looping is recursive in Clojure, the loop construct is a hack so that something like tail-recursive-optimization works in clojure.
user=> (defn my-re-seq [re string]
         "Something like re-seq"
         (let [matcher (re-matcher re string)]

           (loop [match (re-find matcher) ;loop starts with 2 set arguments
                  result []]
             (if-not match
               result
               (recur (re-find matcher)    ;loop with 2 new arguments
                      (conj result match))))))

#'user/my-re-seq

user=> (my-re-seq #"\d" "0123456789")
["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]

;tail position: http://stackoverflow.com/questions/7813497/clojure-what-exactly-is-tail-position-for-recur
; http://stackoverflow.com/questions/7052203/how-can-i-call-recur-in-an-if-conditional-in-clojure
(fn t [x] (loop [c 0 x x] (when (< c x) (println "run") (recur (+ 1 c) x)))))

(loop [i 0] (if (< i 10) (recur tail (+ 1 i)) (print "end")))
(loop [i 0] (when (< i 10) (print i) (recur (+ 1 i)) ))
(loop [x 10] (when (> x 1) (println x) (recur (- x 2)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(
	lazy-seq
		clojure.core

    (lazy-seq & body)

	Takes a body of expressions that returns an ISeq or nil, and yields
	a Seqable object that will invoke the body only the first time seq
	is called, and will cache the result and return it on all subsequent
	seq calls.
)

(println "Fibonacci Script")

; fib_n = fib_n-1 + fib_n-2
(defn fibonacci [a b]
	(lazy-seq 
		(cons a (fibonacci b (+ a b))
		)
	)
)
; (take n (fibonacci 1 1))
; (println (take 9 (fibonacci 1 1)))
(defn useFib [x] (
	take x (fibonacci 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn useFib [x] (
	take x (
		(fn fib [a b] (
			lazy-seq (
				cons a (
					fib b (+ a b))))) 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn useFib [x] ( take x ( (fn fib [a b] ( lazy-seq ( cons a ( fib b (+ a b))))) 1 1)))

;;;;;;;;;;;;;;;;;;;;;;

; palindrome checker
; #(= (seq %) (reverse %))

(fn palCheck [coll] (
	= (seq coll) (reverse coll)
)

;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:doc "user structure"} user {:name "mr.foo", :role "token", :id 0}) 
(let [a 1, b 2] (+ a b))

(let [a (take 5 (range))
             {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
             [e f g & h] ["a" "b" "c" "d" "e"]
             _ (println "I was here!")
             foo 12
             bar (+ foo 100)]
         [a b c d e f g h foo bar])
;The underscore inside the brackets tells Clojure that we expect that first parameter to be there
;I was here!
;[(0 1 2 3 4) 20 50 100 "a" "b" "c" ("d" "e") 12 112]

(letfn [(twice [x] (* x 2))
        (six-times [y] (* (twice y) 3))]
         (println "Twice 15 =" (twice 15))
         (println "Six times 15 =" (six-times 15)))
;Twice 15 = 30
;Six times 15 = 90
;nil
;; Unable to resolve symbol: twice in this context
; user=> (twice 4)
; Evaluation aborted.
;; Unable to resolve symbol: six-times in this context
; user=> (six-times 100)
; Evaluation aborted.

(let [url "http://www.cnn.com"
      goose (Goose. (Configuration.))
      article (.extractContent goose url)]
  (println (.cleanedArticleText article))) 

;;;;;;;;;;;;;;;;;;;;;;;

#_(
; Defines a function (fn). Fns are first-class objects that implement the IFn interface. 
; The IFn interface defines an invoke() function that is overloaded with arity ranging from 0-20.
; A single fn object can implement one or more invoke methods, and thus be overloaded on arity.
; One and only one overload can itself be variadic, by specifying the ampersand followed by a single rest-param.
; Such a variadic entry point, when called with arguments that exceed the positional params, will find them in a seq contained in the rest param.
)

(def mult
  (fn this
      ([] 1)
      ([x] x)
      ([x y] (* x y))
      ([x y & more]
          (apply this (this x y) more))))

;;;;;;;;;;;;;;;;;;;;;;;;

#_(
when-let
clojure.core

    (when-let bindings & body)
    ; & : ampersand denotes the entry point for a variadic arguments
    ; then, last, the body
bindings => binding-form test

When test is true, evaluates body with binding-form bound to the value of test
)

(defn dropOne [coll] 
  (when-let
    [s (seq coll)] ; the binding-form test
    (rest s) ; the body
  ))

(let [a 1, b 2]
  (list a b))
;;=> (1 2)

(let [[a b c] "foo"]
  (list a b c))
;;=> (\f \o \o)

      ; binding form      ; init expression
(let [{a :keyA, b :keyB}  {:keyA 1, :keyB 2}]
  (list a b))
;;=> (1 2)

(defn foo [{a :keyA, b :keyB}]
  (list a b))

(foo {:keyA 1 :keyB 2})
;;=> (1 2)

; if you want the local variables to have the same names as the keywords in the initialization expressions, the is the special key in the binding map called :keys.
(defn foo [{:keys [a b]}]
  (list a b))

(foo {:a 5, :b 6})
;;=> (5 6)

; You can also supply a map of default values using the special key in the binding map, the :or key
(defn foo [{:keys [a b c], :or {c 42}}]
  (list a b c))

(foo {:a 7, :b 8, :c 9})
;;=> (7 8 9)

(foo {:a 20, :b 30})
;;=> (20 30 42)

; pre and post conditions...
(defn foo [{:keys [a b c]}]
  {:pre [(not (nil? c))]}
  (list a b c))

(foo {:a 1, :c 3})
;;=> (1 nil 3)

(foo {:b 2})
;;=> java.lang.AssertionError: Assert failed: (not (nil? c))

;;;;;;;;;;;;;;;;;;;;;;;;

#_(

binding
clojure.core

    (binding bindings & body)

binding => var-symbol init-expr

Creates new bindings for the (already-existing) vars, with the
supplied initial values, executes the exprs in an implicit do, then
re-establishes the bindings that existed before. The new bindings
are made in parallel (unlike let); all init-exprs are evaluated
before the vars are bound to their new values.
)

;; Here are the definitions.
(defn mymax [x y]
  (min x y))

(defn find-max [x y]
  (max x y))

user=> (let [max mymax]
         (find-max 10 20))
20 ;let is ineffective outside current lexical scope

user=> (binding [max mymax]
         (find-max 10 20))
10 ;because max is now acting as min

;; As of Clojure 1.3, vars need to be explicitly marked as ^:dynamic in order for
;; them to be dynamically rebindable:
user=> (def ^:dynamic x 1)
user=> (def ^:dynamic y 1)
user=> (+ x y)
2

;; Within the scope of the binding, x = 2 and y = 3
user=> (binding [x 2 y 3]
         (+ x y))
5

;; But once you leave the binding's scope, x and y maintain their original
;; bindings:
 user=> (+ x y)
 2

;;;;;;;;;;;;;;;;;;;;;;;;

(defn flatten2
  "Like `clojure.core/flatten` but better, stronger, faster.
  Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat, lazy sequence.
  If the argument is non-sequential (numbers, maps, strings, nil, 
  etc.), returns the original argument."
  {:static true}
  [x]
  (letfn [(flat [coll] 
                  (lazy-seq 
                   (when-let [c (seq coll)] 
                     (let [x (first c)] 
                       (if (sequential? x) 
                         (concat (flat x) (flat (rest c))) 
                         (cons x (flat (rest c))))))))]
    (if (sequential? x) (flat x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; declare a function flatten2 that
; takes an argument x.
(fn flatten2 [x] 
  ; here I define a function
  ( letfn flattenBody [
    (flat [coll] 
    ; declaring here a function flat that takes a collection
    ; this function flat works on a lazy-seq created by a when-let
      (lazy-seq 
        (when-let 
          ; the when-let evaluates body with this binding-form test 
          ; bound to the value of test, i.e. when coll is a collection
          [c (seq coll)]
          ; from this point c is a seq resulting from the coll param 
            (let [x (first c)] 
              ; evaluate the first element of c
              (if (sequential? x) 
                 ; if coll implements sequential then
                 (concat (flat x) (flat (rest c)))
                 ; else
                 (cons x (flat (rest c)))
              )
            )
          )
        )
      )
    ]
    (if (sequential? x) (flat x) x)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 28: Write a function which flattens a sequence.
; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
; forbidden: flatten

(fn fltnn[coll]
  (let [a (first coll), b (next coll)]
    (concat 
      (if (sequential? a)
        (fltnn a)
        [a]
      )
      ; using (concat x y) and y is evaluated only when sequential? b
      ; else stop
      (when (sequential? b)
        (fltnn b)
      )
    )
  )
)

; we basically treat the nested collection as a tree and recursively walk the
; tree. Clojure's flatten use a tree-seq to walk the tree.

;;;;;;;

(fn f [[h & t]] 
  (if h
    (if (coll? h)
      (concat (f h) (f t))
      (cons h (f t)))))

(fn [ss]
  (letfn [(flt [s]
              (if (coll? s)
                (apply concat (map flt s))
                [s]))]
   (flt ss)))

(fn [s]
  (filter (complement sequential?)
    (tree-seq sequential? seq s)))

;;;;;;;

#_(

apply
clojure.core

    (apply f args)
    (apply f x args)
    (apply f x y args)
    (apply f x y z args)
    (apply f a b c d & args)

Applies fn f to the argument list formed by prepending intervening arguments to args.
)

(fn takeCapitals [s] 
  (apply str
    (filter #(Character/isUpperCase %) (seq s)
    )
  )
)

;; lol version
(fn takeCapitalsRE [s]
  (
    apply str (seq (clojure.string/split s #"[a-z , ! $ # & * ( 0-9]"))
  )
)

#(apply str (re-seq #"[A-Z]" %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; remove only the consecutive duplicates from a sequence.

(fn rmcons [collection] 
  (when-let [[head & rest] (seq collection)] ; the binding form
    ; in this binding form the head of the collection is bound
    ; to head and eventually the rest of collection is bound
    ; to rest.
    ; the when-let evaluates the body(s) only if the collection
    ; is not empty, and eventually the rest is in rest.	
    (if (= head (first rest))
      (rmcons rest)
      (cons head (rmcons rest))
     )
    ; if head have a dupes as next, head will be discarded
    ; otherwise head is consed to the result, and rest is evaluated
  )
)

;;;;;;;;;;;;;;;;;;;;

(fn lol [a b & c] (println a) (println b) (println ( seq c))) "foo" "bar" "foobar" "lol")

;;;;;;;;;;;;;;;;;;;;;

((fn foo [x]
   (loop [i 0]
     (println (str "running... " i))
         (if (= i x)
       (println (str i ":" x " end"))
       (recur (inc i))
  )
)
  ) 5)

;;;;;;;;;

  (defn multiple? [n div]
    (= 0 (mod n div)))

;;;;;;;;;;;;;

  (defn factorial-1 [number]
  "computes the factorial of a positive integer
   in a way that doesn't consume stack space"
  (loop [n number factorial 1]
    (if (zero? n)
      factorial
      (recur (dec n) (* factorial n)))))

;;;;;;;;;;;

;; another fib

(fn [n]
  (take n
        (map first
             (iterate (fn [[f s]] [s (+ f s)])
                      [1 1]))))

((fn [n] (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))) 5)

;;;;;;;;;;;;;;;;;;

; 30: Write a function which removes consecutive duplicates from a sequence.
;  (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(fn cmprs [coll]
  (when-let [[f & r] (seq coll)] ;run while (seq coll) != nil
    (if (= f (first r)) 
      (cmprs r) 
      (cons f (cmprs r)))))  
; Basically a variant of the filter function. Note the sequence is destructed
; into first element f and the rest r. 

;Why we need of (seq coll)?
;coll? returns true if x implements IPersistentCollection
;and coll? "foo" is false but coll? (seq "foo") is true.

; because (seq nil) returns nil, the binding-form test [f (seq coll)] is a good boolean test for the when-let form.


(fn [a]
  (map first (partition-by identity a)))

;;;;;;;;;;;;;;;;

;;;;

(defn make-point [x y]
  (fn [member]
    (cond
          (= member :x) x
          (= member :y) y)))

;;;;

(defn make-point [x y]
  (fn [f]
    (f x y)))

(defn point-x [pt]
  (pt (fn [x y] x)))
(defn point-y [pt]
  (pt (fn [x y] y)))
(def pt (make-point 1 2))
(println [(point-x pt) (point-y pt)])

;;;;;;;;;;

user=> (for [x [0 1 2 3 4 5]
             :let [y (* x 3)]
             :when (even? y)]
         y)
;=> (0 6 12)

user=> (for [x (range 1 6)
             :let [y (* x x)
                   z (* x x x)]]
         [x y z])

;=> ([1 1 1] [2 4 8] [3 9 27] [4 16 64] [5 25 125])

; Demonstrating difference between :when and :while

user=> (time (dorun (for [x (range 1000) y (range 10000) :when (> x y)] [x y])))
;=> "Elapsed time: 2898.908 msecs"
;=> nil

user=> (time (dorun (for [x (range 1000) y (range 10000) :while (> x
y)] [x y])))
;=> "Elapsed time: 293.677 msecs"
;=> nil


(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))

((defn print-seq [s]
  (if (sequential? s)
        (doseq [z (map inc s)]
      (prn z)))
        (str "started from: " s)
   ) '( 1 2 3 ))

(doseq [x [1 2 3]
        y [1 2 3]]
         (prn (* x y)))

(doseq [x '(1 2 3)]
  (prn x))

  (#( = (first %) (first (next (seq %))))"fooof")

(if ( true) (pr "yes")(prn "no"))

user=> (assoc {} :key1 "value" :key2 "another value")
{:key2 "another value", :key1 "value"}

user=> (assoc {:key1 "old value1" :key2 "value2"} :key1 "value1" :key3 "value3")
{:key3 "value3", :key2 "value2", :key1 "value1"}

user=> (assoc [1 2 3] 0 10)
[10 2 3]

user=> (assoc [1 2 3] 3 10)
[1 2 3 10]

(#( = (first %) (first (next (seq %)))) "fooof")

;;;;;;;;;;;;;;;;;;;;;;;

((fn fs [x]
  (if
    (#( = (first %) (first (next (seq %)))) x)
    (pr "yes")
    (pr "no")
        ;(str (first x) ":" (fs (rest x)))
        ))
"foof")

;;;

; 30. compress a sequence
(fn rd [c] 
  (when-let [s (seq c)]
    (if (= (first s) (first (rest s)))
    (rd (rest s))
    (cons (first s) (rd (rest s))))))

; using partition
#_(
partition-by
clojure.core
    (partition-by f coll)
Applies f to each value in coll, splitting it each time f returns
a new value. Returns a lazy seq of partitions.
)

(fn rd [c] (map distinct (partition-by (fn [x] x) (seq c))))
((fn rd [c] (flatten (map distinct (partition-by (fn [x] x) (seq c))))) [1 1 2 3 4 4 4 ])
((fn rd [c] (apply str (flatten (map distinct (partition-by (fn [x] x) (seq c)))))) "fooooobas")

;;;;;;;;;;;;;;

; 31. pack a sequence

; the main function
(fn [coll] 
  ; the inner function pack for the recursion
  ; pack takes 3 arguments: res, prev, coll
  ((fn pack [res prev coll]
    ; (if-let bindings then)
    ;  
    (if-let [[f & r] (seq coll)] 
      (if (= f (first prev)) 
         (pack res (conj prev f) r) 
         (pack (conj res prev) [f] r))) 
    (conj res prev))
    ; first argument for pack 
    [] ; res
    ; second argument for pack
    [(first coll)] ; prev
    ; third argument for pack
    (rest coll)) ; coll
) 

