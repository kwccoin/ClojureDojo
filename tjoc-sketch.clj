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
; user=> (doc nth)
; -------------------------
; clojure.core/nth
; ([coll index] [coll index not-found])
;   Returns the value at the index. get returns nil if index out of
;   bounds, nth throws an exception unless not-found is supplied.  nth
;   also works for strings, Java arrays, regex Matchers and Lists, and,
;   in O(n) time, for sequences.
; nil
; user=> (doc map)
; -------------------------
; clojure.core/map
; ([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
;   Returns a lazy sequence consisting of the result of applying f to the
;   set of first items of each coll, followed by applying f to the set
;   of second items in each coll, until any one of the colls is
;   exhausted.  Any remaining items in other colls are ignored. Function
;   f should accept number-of-colls arguments.
; nil
; user=> (doc reduce)
; -------------------------
; clojure.core/reduce
; ([f coll] [f val coll])
;   f should be a function of 2 arguments. If val is not supplied,
;  returns the result of applying f to the first 2 items in coll, then
;  applying f to that result and the 3rd item, etc. If coll contains no
;  items, f must accept no arguments as well, and reduce returns the
;  result of calling f with no arguments.  If coll has only 1 item, it
;  is returned and f is not called.  If val is supplied, returns the
;  result of applying f to val and the first item in coll, then
;  applying f to that result and the 2nd item, etc. If coll contains no
;   items, returns val and f is not called.
; nil
; user=> (doc filter)
; -------------------------
; clojure.core/filter
; ([pred coll])
;   Returns a lazy sequence of the items in coll for which
;   (pred item) returns true. pred must be free of side-effects.
; nil
; user=> (doc drop-while)
; -------------------------
; clojure.core/drop-while
; ([pred coll])
;  Returns a lazy sequence of the items in coll starting from the first
;   item for which (pred item) returns nil.
; user=> (doc delay)
; -------------------------
; clojure.core/delay
; ([& body])
; Macro
;  Takes a body of expressions and yields a Delay object that will
;  invoke the body only the first time it is forced (with force or deref/@), and
;   will cache the result and return it on all subsequent force
;  calls.
; nil
; user=> (doc force)
; -------------------------
; clojure.core/force
; ([x])
;   If x is a Delay, returns the (possibly cached) value of its expression, else returns x
; nil
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

; Neighbours function.
; Given a point 'xy' and a 'size'
;neighbors return the vector of the
;cross coordinates surround the point
;and filtered by the existence.
(defn neighbors 
  ; deltas are the 'cross' coordinates to check
  ; xy are the coordinates of a point
  ; size, the distance to check
  ([size xy] 
    (let [deltas [[-1 0][1 0][0 -1][0 1]]] (neighbors deltas size xy)))
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
; "from the current point [x y], calculate the expected cost by
; assuming we can travel to the right edge, the down to the lower-right."
(defn estimate-cost [step-cost-est size x y]
  (* step-cost-est
    (- (+ size size) y x 2)))

(estimate-cost 900 5 0 0)
;=> 7200

; 7.10 - A g function used to calculate the cost of the path traversed
; so far. Add cheapest neighbor cost, else 0.
(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
    (:cost cheapest-nbr 0)))

(path-cost 900 {:cost 1})
;=> 901

; 7.11 - A f function to calculate the estimated cost of the path (+ (g ...) (h ...))
(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
    (estimate-cost step-cost-est size y x)))

(total-cost 1000 900 5 3 4)
;=> 1900

; Three functions:
; - h: estimated cost
; - g: current cost
; - f: total cost

; Auxiliary function
(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
      (if (> (f min) (f this)) this min))
    coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 42}])

; The A* algorithm implemented as tail-recursive solution.
; 7.12 - the main A* algorithm
(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [;the number of steps 
           steps 0
           ;the matrix of routes
           routes (vec (replicate size (vec (replicate size nil))))
           ; The soul of A* is based on the fact that the
           ; potential paths stored in work-todo are always
           ; sorted and distinct, based on the estimated 
           ; path cost function
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo) ;check done
        ; if the work-todo is empty the algorithm ends here and
        ; return the first route and the number of steps
        [(peek (peek routes)) :steps steps] ;grab first route
        ; the prepare the variables and start searching...
        (let [
              ; the work-item is a point to start from
              ; skipping the first element of the work-todo item
              [_ yx :as work-item] (first work-todo)
              ; remove the work-item from the work-todo
              rest-work-todo (disj work-todo work-item)
              ; nbr-yxs contains the list of neighbors
              nbr-yxs (neighbors size yx)
              ; considering the neighbors, keeping the yx in routes,
              ; take the one with minimum cost
              cheapest-nbr (min-by :cost (keep #(get-in routes %)
                                           nbr-yxs))
              ; updating the new path-cost
              ; taking the yx's cost from cell-costs.
              ; newcost is the cost so far for the cheapest neighbor
              newcost (path-cost (get-in cell-costs yx) cheapest-nbr)
              ; save the current cost, the cost-so-far
              oldcost (:cost (get-in routes yx))]
          ; with the variables setted, check if the newcost
          ; is greater than the oldcost.
          ; This is the main check of the A*
          (if (and oldcost (>= newcost oldcost))
            ; newcost is greater the oldcost, so throw away this new path
            ; beacause its clearly a wore alternative.
            (recur 
              (inc steps) 
              routes 
              rest-work-todo)
            ; core functionality: constant sort of the work-todo, based
            ; on the cost og the path as determined by the heuristica
            ; function total-cost.
            (recur 
              (inc steps)
              (assoc-in routes yx
                {:cost newcost
                 :yxs (conj (:yxs cheaperst-nbr []) yx)
                })
              ; add estimated path to todo and recur
              ; Each recursive loop through the astar function
              ; maintains the sorted routes based on the current
              ; cost knowledge of the path, added to the estimated
              ; total cost.
              (into rest-work-todo
                (map 
                  (fn [w]
                    (let [
                      [y x] w]
                      [(total-cost newcost step-est size y x) w])
                  )
                  nbr-yxs)
              )    
            )
          )
        )
      )
    )
  )
)

(astar [0 0] 900 world)
; tjcl=> [{
;   :cost 17,
;   :yxs [[0 0] [0 1] [0 2] [0 3] [0 4] [1 4] [2 4]
;       [2 3] [2 2] [2 1] [2 0] [3 0] [4 0] [4 1]
;       [4 2] [4 3] [4 4]]}
;   :steps 94]

; Clojure favors an approach where immutable data is transformed
; through the application of functions.
; Additionally, Clojure prefers that functions be free of side-effects
; and referentially transparent (pure) in order to reduce the
; complexities inherent in widespread data mutation.
; Lexical closures provide a simple yet powerful mechanism for defining
; functions that cassy around with them the value context in which they
; were created.
; Clojure primary form of iteration is through tail recursion as natural
; result of its focus on immutability.

; tjoc=> (doc peek)
; -------------------------
; clojure.core/peek
; ([coll])
;  For a list or queue, same as first, for a vector, same as, but much
;  more efficient than, last. If the collection is empty, returns nil.
; nil
; tjoc=> (peek (list 1 2 3 4))
; 1
; tjoc=> (peek (vector 1 2 3 4))
; 4
; tjoc=> (doc vec)
; -------------------------
; clojure.core/vec
; ([coll])
;   Creates a new vector containing the contents of coll.
; nil
; tjoc=> (doc vector)
; -------------------------
; clojure.core/vector
;([] [a] [a b] [a b c] [a b c d] [a b c d & args])
;   Creates a new vector containing the args.
; nil
; tjoc=> (doc replicate)
; -------------------------
; clojure.core/replicate
; ([n x])
;   Returns a lazy seq of n xs.
; nil 
; tjoc=> (get-in {:a {:b {:c {:foo "found!"}}}} [:a :b :c :foo])
; "found!"
; tjoc=> (doc get-in)
; -------------------------
; clojure.core/get-in
; ([m ks] [m ks not-found])
;   Returns the value in a nested associative structure,
;   where ks is a sequence of ke(ys. Returns nil if the key is not present,
;   or the not-found value if supplied.
; nil
; tjoc=> (doc sorted-set)
; -------------------------
; clojure.core/sorted-set
; ([& keys])
;   Returns a new sorted set with supplied keys.
; nil
; tjoc=> (sorted-set [0 2] [4 2] [10 3] [4 3] [9 0])
; #{[0 2] [4 2] [4 3] [9 0] [10 3]}
; tjoc=> (doc disj)
; -------------------------
; clojure.core/disj
; ([set] [set key] [set key & ks])
;  disj[oin]. Returns a new set of the same (hashed/sorted) type, that
;  does not contain key(s).
; nil
; tjoc=> (doc keep)
; -------------------------
; clojure.core/keep
; ([f coll])
;  Returns a lazy sequence of the non-nil results of (f item). Note,
;  this means false return values will be included.  f must be free of
;  side-effects.
; nil
; tjoc=> (doc assoc)
; -------------------------
; clojure.core/assoc
; ([map key val] [map key val & kvs])
;   assoc[iate]. When applied to a map, returns a new map of the
;     same (hashed/sorted) type, that contains the mapping of key(s) to
;     val(s). When applied to a vector, returns a new vector that
;    contains val at index. Note - index must be <= (count vector).
; nil
; tjoc=> (doc into)
; -------------------------
; clojure.core/into
; ([to from])
;   Returns a new coll consisting of to-coll with all of the items of
;   from-coll conjoined.
; nil

; 8.1.1 - Macros
; An implementation of eval taking a local centext
(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))
(contextual-eval {'a 1, 'b 2} '(+ a b))
;tjof=> 3

; Quote (')
;
; (quote form)
; Yields the unevaluated form.
; 
; user=> '(a b c)
; (a b c)
; 
; Syntax-quote (`, note, the "backquote" character), Unquote (~) and
; Unquote-splicing (~@)
; 
; The syntax-quote (`, the backquote character).
; For Symbols.
; It resolves the Symbol in the current context, yielding a
; fully-qualified symbol.
; For List/Vectors/Set/Maps.
; It establishes a template of the corresponding data structure. Within
; the template, unqualified forms behave as if recursively
; syntax-quoted, but forms can be exempted from such recursive quoting
; by qualifying them with unquote or unquote-splicing, in which case the
; will be treated as expressions and be replaced in the template by
; their value, or sequence of values, respectively.
; 
; For example:

(def x 5)
(def lst '(a b c))
`(fred x ~x lst ~@lst 7 8 :nine)
;=> (user/fred user/x 5 user/lst a b c 7 8 :nine)

; ---------------------------
(let [x 3, y '(- x)]
  (println `y)
  (println ``y)
  (println ``~y)
  (println ``~~ y)
  (contextual-eval {'x 42} ``~~y))
; user/y
; (quote user/y)
; user/y
; (- x)
; 42
;-----------------------------
(list 'println x (eval x) y)
; tjoc=> (println (* 3 5) 15 15)
(list `println x (eval x) y)
; tjoc=> (clojure.core/println (* 3 5) 15 15)
`(list println x (eval x) y)
; tjoc=> (clojure.core/list clojure.core/println tjoc/x
; (clojure.core/eval tjoc/x) tjoc/y)
`(println x (eval x) y)
; tjoc=> (clojure.core/println tjoc/x (clojure.core/eval tjoc/x) tjoc/y)
`(println ~x (eval x) y)
; tjoc=> (clojure.core/println (* 3 5) (clojure.core/eval tjoc/x) tjoc/y)
`(println ~x ~(eval x) y)
; tjoc=> (clojure.core/println (* 3 5) 15 tjoc/y)
`(println ~x ~(eval x) ~y)
; (clojure.core/println (* 3 5) 15 15)
`(println ~x ~(eval x) ~y ~@x)
; (clojure.core/println (* 3 5) 15 15 * 3 5)
;-----------------------------

; 8.2.1 - Defining control structures without syntax-quote.
; Because the arguments to defmacro aren't evaluated before
; being passed to the macro, they can be viewd as pure data
; structures, and manipulated and analyzed as such.

(defmacro do-until [& clauses]
  (when clauses
    (list `when (first clauses)
      (if (next clauses)
        (second clauses)
        (throw (IllegalArgumentException.
          "do-until requires an even number of forms")))
      (cons 'do-until (nnext clauses)))))

; Listing 8.2 - A Clojure implementation of unless
; syntax-quote: (`)
; unquote: (~)
; unquote-splice (~@)

(defmacro unless [condition & body]
  `(if (not ~condition) ; unquote condition
    (do ~@body))) ; splice body
(defn from-end [s n]
  (let [delta (dec (- (count s) n))]
    (unless (neg? delta)
      (nth s delta))))
(from-end (range 1 101) 10)
;=> 90

; Syntax-quote allows the if form to act as a template for
; the expression that any use of the macro becomes when expanded.
; The unquote and splicing-unquote provide the "blanks" where the
; values for the parameters condition and body will be inserted.
; The unquote for condition is imperative. If we didn;t use
; unquote in this instance, the instead of evaluating a function
; (even? 3), it would instead attemp to resolve a namespace Var
; named condition, if exists.
;
; Side note: (defmacro unless [& args] `(when-not ~@args))

; Note:
; - do-until: the template starts from the where form and it's correct
; to search variables named clause because that will exists when the
; macro will be expanded but aren't necessary now, during the
; definition.
; We just want to setup the recursion, not to call it, so 'do-until
; yeild a form that must remain unevaluated.
; - unless: the template starts from the if form and we want to
; evaluate the condition, thus is unquoted.

(ns macro-sketch 
(:require [clj-time.core :as ctime]))
(defmacro sketch-macro [foo bar]
  `(do 
      (println (ctime/now))
      (println "waiting..." ) 
      (if ; the template starts here...
        (and ~foo ~bar)
        (let [seed# (rand)]
          (println (ctime/now))
          {:seed seed#})
        (:not-and))))

(defn sketch-fn [foo bar]
  (do 
    (println (ctime/now))
    (println "waiting..." ) 
    (if ; the template starts here...
      (and foo bar)
        (let [seed# (rand)]
          (println (ctime/now))
          {:seed seed#})
        (:not-and))))
; function's arguments are evaluated
; before the function is called.
; With this macro, we see immediatly the waiting print...
(sketch-macro (= 1 1) (do (Thread/sleep 3000) true))
; Instead, with a simple function, the first argument
; is evaluated before the function call...
; so we are forced to wait 3 seconds before see the
; waiting print.
(sketch-fn (= 1 1) (do (Thread/sleep 3000) true))
;
; macro-sketch=> (sketch-macro (= 1 1) (do (Thread/sleep 3000) true))
; #<DateTime 2012-09-25T20:06:54.060Z>
; waiting...
; #<DateTime 2012-09-25T20:06:57.066Z>
; {:seed 0.8320156852880689}
;
; macro-sketch=> (sketch-fn (= 1 1) (do (Thread/sleep 3000) true))
; #<DateTime 2012-09-25T20:07:08.460Z>
; waiting...
; #<DateTime 2012-09-25T20:07:08.460Z>
; {:seed 0.826365358151938}

;user=> (doc keep-indexed)
;-------------------------
;clojure.core/keep-indexed
;([f coll])
;  Returns a lazy sequence of the non-nil results of (f index item). Note,
;  this means false return values will be included.  f must be free of
;  side-effects.
;nil
;user=> (doc add-watch)
;-------------------------
;clojure.core/add-watch
;([reference key fn])
;  Alpha - subject to change.
;  Adds a watch function to an agent/atom/var/ref reference. The watch
;  fn must be a fn of 4 args: a key, the reference, its old-state, its
;  new-state. Whenever the reference's state might have been changed,
;  any registered watches will have their functions called. The watch fn
;  will be called synchronously, on the agent's thread if an agent,
;  before any pending sends if agent or ref. Note that an atom's or
;  ref's state may have changed again prior to the fn call, so use
;  old/new-state rather than derefing the reference. Note also that watch
;  fns may be called from multiple threads simultaneously. Var watchers
;  are triggered only by root binding changes, not thread-local
;  set!s. Keys must be unique per reference, and can be used to remove
;  the watch with remove-watch, but are otherwise considered opaque by
;  the watch mechanism.
;nil

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

(def-watched x (* 12 12))
x
;=> 144
(def x 0)
;=> 144 -> 0

;user=> (doc declare)
;-------------------------
;clojure.core/declare
;([& names])
;Macro
;  defs the supplied var names with no bindings, useful for making forward declarations.

; Listing 8.4 - Domain DSL's underlying form
{:tag <node form>,
 :attrs {},
 :content [<nodes>]}
(defmacro domain [name & body]
  `{:tag :domain,
    :attrs {:name (str '~name)},
    :content [~@body]})
(declare handle-things)
(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str '~name)},
    :content [~@(handle-things body)]})
(declare grok-attrs grok-props)
(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))
(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))
(defn grok-props [props]
  (when props
    {:tag :properties, :attrs nil,
     :content (apply vector (for [p props]
                 {:tag :property,
                  :attrs {:name (str (first p))},
                  :content nil}))}))
(def d
  (domain man-vs-monster
    (grouping people  ;  Group of people
      (Human "A stock human")
      (Man (isa Human)
         "A man, baby"
         [name]
         [has-beard?]))
    (grouping monsters ;  Group of monsters
      (Chupacabra      ;  One kind of monster
         "A fierce, yet elusive creature"
         [eats-goats?]))))
(:tag d)
;=> :domain
(:tag (first (:content d)))
;=> :grouping

(use '[clojure.xml :as xml])
(xml/emit d)

; 8.5 - Using macro to control symbolic resolution time
(defmacro resolution [] `x)
(macroexpand '(resolution))
;=> user/x


; ---
