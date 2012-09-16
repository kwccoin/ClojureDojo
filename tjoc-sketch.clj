; reading The Joy of Clojure

; 6.3.4
; Infinite sequences foster declarative solutions

(defn triangle [n]
  (/ (* n (+ n 1)) 2))
(def tri-nums (map triangle (iterate inc 1)))
(take 10 tri-nums)
(take 10 (filter even? tri-nums))
(nth tri-nums 99)

(double (reduce + (take 1000 (map / tri-nums))))

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
user=> (doc filter)
-------------------------
clojure.core/filter
([pred coll])
  Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects.
nil
user=> (doc drop-while)
-------------------------
clojure.core/drop-while
([pred coll])
  Returns a lazy sequence of the items in coll starting from the first
  item for which (pred item) returns nil.
user=> (doc delay)
-------------------------
clojure.core/delay
([& body])
Macro
  Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls.
nil
user=> (doc force)
-------------------------
clojure.core/force
([x])
  If x is a Delay, returns the (possibly cached) value of its expression, else returns x
nil
)

;6.3.5 - call-by-need semantics
(defn defer-expensive [cheap expensive] ;cheap and expensive are Delay objects
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))
(defer-expensive 
  (delay :cheap) ;first parameter, the cheap (true)
  (delay (do (Thread/sleep 5000) :expensive)) ;second parameter
  )
;=> :cheap
(defer-expensive
  (delay false)
  (delay (do (Thread/sleep 5000) :expensive)) ;second parameter
)
;=> :expensive

