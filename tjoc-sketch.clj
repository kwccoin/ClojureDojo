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

; 7.1.4 - Named Arguments
(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  ; &      variadic parameter in a seq
  ; :as    bind a local to the entire collection
  ; :keys  the next form will be a vector of names the it should
  ;        convert to keywords
  ; :strs  Clojure would be looking for items in the map with string keys
  ; :syms  for symbol keys
  ; :or    if a key is not in the map, the default value is this
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(defn slope [p1 p2]
  {:pre [(not = p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

; Functional Programming - 7.2
; Sharing closure context

(def bearing [{:x  0, :y  1} ; north 0#
              {:x  1, :y  0} ; east  1#
              {:x  0, :y -1} ; south 2#
              {:x -1, :y  0} ; west  3#
             ])

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(forward 5 5 0)
;=> [5 6]

; multiple closures sharing the same environment

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south: west] bearing-num)
   :forwar (fn [] (bot (+ x (:x (bearings bearing-num)))
                       (+ y (:y (bearings bearing-num)))
                       bearing-num))
   :turn-right (fn [] (boc x y (mod (+ 1 bearing num) 4)))
   :turn-left  (fn [] (boc x y (mod (- 1 bearing num) 4)))
  })

; Listing 7.6 - Using mutually recursive functions to implement a 
; finite state machine (FSA)
(defn elevator [commands]
  (letfn 
    [(ff-open [[cmd & r]]
      "When the elevator is open on the 1st floor
       it can either close o be done."
       #(case cmd
          :close (ff-closed r)
          :done   true
          false))
      (ff-closed [[cmd & r]]
        "When the elevator is closed on the 1st floor
         it can either open or go up."
         #(case cmd
            :open (ff-open r)
            :up   (sf-closed r)
            false))
       (sf-closed [[cmd & r]]
         "When the elveator is colosed on the 2nd floor
          it can either go down or open."
          #(case cmd
             :down (ff-closed r)
             :open (sf-open r)
             false))
        (sf-open [[cmd & r]]
          "When the elevator is open on the 2nd floorr
           it can either close or be done."
           #(case cmd
              :close (sf-closed r)
              :done   true
              false))]
     (trampoline ff-open commands)))

(elevator [:close :open :close :up :open :open :done])
;=> false

(elevator [:close :up :open :close :down :open :done])
;=> true


#_(

trampoline
clojure.core

    (trampoline f)
    (trampoline f & args)

trampoline can be used to convert algorithms requiring mutual
recursion without stack consumption. Calls f with supplied args, if
any. If f returns a fn, calls that fn with no arguments, and
continues to repeat, until the return value is not a fn, then
returns that non-fn value. Note that if you want to return a fn as a
final value, you must wrap it in some data structure and unpack it
after trampoline returns.)

; Rules for the mutual recursion (general rules)
; 1. Make all the functions participating in the mutual recursion return a function instead of their normal result. Normally this is as simple as tacking a # onte the front of the outer level of the function body.
; 2. Invoke the first function in the mutual chain via the trampoline function.
; The trampoline function handles the process of the self calls through the placement of the functions within a list where each function is "bounced" cack and forth explicitly.

; ----------------------------

; 7.4 - A* pathfinding

;given a point 'xy' and a 'size'
;neighbours return the vector of the
;cross coordinates surround the point
;and filtered by the existence.

(defn neighbours 
  ; deltas are the 'cross' coordinates to check
  ; xy are the coordinates of a point
  ; size, the distance to check
  ([size xy] 
    (let [deltas [[-1 0][1 0][0 -1][0 1]]] (neighbours deltas size xy)))
  ([deltas size xy]
    (filter 
    ;filter needs a predicate, so I define one here:
      (fn [xys-to-check] (every? #(< -1 % size) xys-to-check))
      ;this predicate just check that every point in xys-to-check, that must be a collection, is inside the 1st quadrant. A predicate must return a Boolean value. It use the '<' function with the 3rd parameter that means the size of the quadrant
    (map #(map + % xy) deltas) 
    ;this map find the vector of cross coordinates and is checked by the predicate i.e. filtered.
)))

(neighbors 5 [0 0])
;=> ([1 0] [0 1])

; 7.9 - A straight-line h function to estimate remaining path cost
(defn estimate-cost [step-cost-est size x y]
  (* step-cost-est
    (- (+ size size) y x 2)))

(estimate-cost 900 5 0 0)
;=> 7200

; 7.10 - A g function used to calculate the cost of the path traversed so far
(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
    (:cost cheapest-nbr 0)))

(path-cost 900 {:cost 1})
;=> 901

; 7.11 - f function to calculate the estimated cost of the path (+ (g ...) (h ...))
(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
    (estimate-cost step-cost-est size y x)))

(total-cost 1000 900 5 3 4)
;=> 1900

; ---
