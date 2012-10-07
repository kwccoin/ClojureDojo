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
; Whereas functions accept and return values that are meaningful
; to your application at runtime, macros accept and return code
; forms that are meaningful at compile time.
(defmacro resolution [] `x)
(macroexpand '(resolution))
;=> user/x
(def x 9)
(let [x 0] (resolution))
;=> 9

; `symbol:  is the syntax-quote that attemo to resolve symbols
; in the current context.
; ~'symbol: avoid that resolution by unquoting a quote.
; This is a bit of awkwarness.

; Listing 8.9 - A more general template for with-open-like macros
(defmacro with-resource [binding close-fn & body]
  `(let ~ binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))
(let [stream (joc-www)]
  (with-resource [page stream]
    #(.close %)
    (.readLine page)))

; ... the use of named bindings marked by vectors is ubiquitous
; and idiomatic in Clojure

; Macros returnin functions
; the "contract" macro.
; The template of a macro contract:
; (contract doubler [x]
;   (:require (positive? x))
;   (:ensure  (= (* 2 x) %)))
; that will return something similar to:
(fn doubler
  ([f x]
    {:post [(= (* 2 x) %)]
     :pre  [(positive x)]}
    (f x)))
; 8.10 - the contract top-level macro.
(declare collect-bodies)
(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))
; In order to follow the multi-arity function definition form
; so that the contract can take more tha one specification per
; arity function, each separated by a vector of symbols:
; (partition represent: arglist, requires and ensures of contract)
(declare build-contract)
(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))
; 8.11 - The contrat auxiliary function build-contract
(defn build-contract [c]
  (let [args (first c)]
    (list
      (into '[f] args)
      (apply merge
        (for [con (rest c)]
          (cond 
            (= (first con) :require)
              (assoc {} :pre (vec (rest con)))
            (= (first con) :ensure)
              (assoc {} :post (vec (rest con)))
            :else (throw (Exception. (str "Unknown tag " (first con)))))))
       (list* 'f args))))
; 8.12 - Composition of contract function and constrained function
(def doubler-contract
  (contract doubler [x]
    (:require 
      (positive? x))
    (:ensure
      (= (* 2 x) %))))
(def times2 (partial doubler-contract #(* 2 %)))
(times2 9)
(def times3 (partial doubler-contract #(* 3 %)))
(times3 9)

;8.13 - Contract for multiple-arity functions
(def doubler-contract
  (contract doubler 
    [x]
      (:require 
        (positive? x))
      (:ensure
        (= (* 2 x) %)
    [x y]
      (:require 
        (positive? x))
        (positive? y))
      (:ensure
        (= (* 2 (+ x y)) %))))
; By using the contract macro, we've provided a way to describe 
; the expectations of a function, including but not limite to:
; - the possible type of its inputs and output
; - the relationship of the function output to its inputs
; - the expected function arities
; - the "shape" of the inputs and output.

; 9.1 - Namespace navigation
; namespace creation: ns, in-ns, create-ns

;user=> (doc ns)
;-------------------------
;clojure.core/ns
;([name docstring? attr-map? references*])
;Macro
;  Sets *ns* to the namespace named by name (unevaluated), creating it
;  if needed.  references can be zero or more of: (:refer-clojure ...)
;  (:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)
;  with the syntax of refer-clojure/require/use/import/load/gen-class
;  respectively, except the arguments are unevaluated and need not be
;  quoted. (:gen-class ...), when supplied, defaults to :name
;  corresponding to the ns name, :main true, :impl-ns same as ns, and
;  :init-impl-ns true. All options of gen-class are
;  supported. The :gen-class directive is ignored when not
;  compiling. If :gen-class is not supplied, when compiled only an
;  nsname__init.class will be generated. If :refer-clojure is not used, a
;  default (refer 'clojure) is used.  Use of ns is preferred to
;  individual calls to in-ns/require/use/import:
;
;  (ns foo.bar
;    (:refer-clojure :exclude [ancestors printf])
;    (:require (clojure.contrib sql sql.tests))
;    (:use (my.lib this that))
;    (:import (java.util Date Timer Random)
;             (java.sql Connection Statement)))
;nil
;user=> (doc in-ns)
;-------------------------
;clojure.core/in-ns
;([name])
;  Sets *ns* to the namespace named by the symbol, creating it if needed.
;nil
;user=> (doc intern)
;-------------------------
;clojure.core/intern
;([ns name] [ns name val])
;  Finds or creates a var named by the symbol name in the namespace
;  ns (which can be a symbol or a namespace), setting its root binding
;  to val if supplied. The namespace must exist. The var will adopt any
;  metadata from the name symbol.  Returns the var.
;nil
;user=> 

(def f (create-ns 'foons))
f
;=> #<Namespace foons>
(intern f 'reduce clojure.core/reduce)
;=> #<'foons/reduce>

; The defn- macro is provided for convenience and simply attaches
; privileged metadata to the Var containing the function.
; You could attach the same namescpace privacy metadata yourself:

(ns hider.ns)
(defn ^{:private true} answer [] 42)
(ns seeker.ns
  (:refer hider.ns))
(answer)
; java.lang.Exception: unable to resolve symbol: answer in this context

; Declarative inclusions and exclusions.
; Fine grained controls:
; Clojure prefers a fine-grained Var mapping via a set of directives
; on the ns macro:
; :exclude, :only, :as, :refer-clojure, :import, :use, :load, :require.

; user=> (doc assoc)
; -------------------------
; clojure.core/assoc
; ([map key val] [map key val & kvs])
;   assoc[iate]. When applied to a map, returns a new map of the
;     same (hashed/sorted) type, that contains the mapping of key(s) to
;     val(s). When applied to a vector, returns a new vector that
;     contains val at index. Note - index must be <= (count vector).
; nil

; 9.2 - Exploring Clojure Multimethods
; with the Universal Design Pattern

(ns joy.udp
  (:refer-clojure :exclude [get]))
; beget
; Takes a map m and associates its prototype
; reference to another map, returning a new map.
(defn beget [m v] (assoc m ::prototype v))
(beget {:sub 0} {:super 1})
;=> {:joy.udp/prototype {:super 1}, :sub 0}
; put
(def put assoc)
; get - implementing the prototype chain
(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))
(get (beget {:sub 0} {:super 1})
  :super)
;=> 1
; remove
; We don't need one because the not found is nit
; and nil is simply false.

(def cat {:likes-dogs true, :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))
(get cat :likes-dogs)
;=> true
(get morris :likes-dogs)
;=> true
(get post-traumatic-morris :likes-dogs)
;=> nil

; Adding behaviors to the UDP can be accomplished easly
; using Clojure's multimethod facilities.
; Multimethos providea way to perform function polymorphism
; based on the result of an arbitrary dispatch function.

#_(
user=> (doc defmulti)
-------------------------
clojure.core/defmulti
([name docstring? attr-map? dispatch-fn & options])
Macro
  Creates a new multimethod with the associated dispatch function.
  The docstring and attribute-map are optional.

  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy
nil
user=> (doc defmethod)
-------------------------
clojure.core/defmethod
([multifn dispatch-val & fn-tail])
Macro
  Creates and installs a new method of multimethod associated with dispatch-value. 
nil
user=>
)

; The multimethod compiler
(defmulti  compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx  [m] (get m :c-compiler))
; if the function compiler is called with a protytype map,
; the the map is queried for an element :os, which has
; methods defined on the results for either ::unix 
; or :osx.
(def clone (partial beget {}))
(def unix {:os ::unix, :c-compiler "cc", 
           :home "/home", :dev "dev"})
(def osx (-> (clone unix)
             (put :os ::osx)
             (put :c-compiler "gcc")
             (put :home "/Users")))
(compiler unix)
;=> "cc"
(compiler osx)
;=> "gcc"

; 9.2.3 Ad hoc hierarchies for inherited behaviors

(defmulti  home :os)
(defmethod home ::unix [m] (get m :home))
(home unix)
;=> "/home"
(home osx)
; java.lang.IllegalArgumentException:
;   No method in multimethod 'home' for dispatch value: :user/osx

; we need a relationship stating: "::osx is a ::unix"
(derive ::osx ::unix)
(home osx)
;=> "/Users"
(parents ::osx)
;=> #{:user/unix}
(ancestors ::osx)
;=> #{:user/unix}
(descendants ::unix)
;=> #{:user/osx}
(isa? ::osx ::unix)
;=> true
(isa? ::unix ::osx)
;=> false
(derive ::osx ::bsd)
(defmethod home ::bsd [m] "/home")
(home osx)
; java.lang.IllegalArgumentException: Multiple methods in
; multimethod 'home' match dispatch value: :user/osx -> :user/unix
; and :user/bsd, nad neither is preferred.
(prefer-method home ::unix ::bsd)
(home osx)
;=> "/Users"
; with prefer-method we explicitly state that for the 
; multimethod home, we prefer the method associated
; with the dispatch value ::unix, over the one for "bsd".

(derive (make-hierarchy) ::osx ::unix)
;=> {:parents {:user/osx #{:user/unix}},
;     :ancestors {:user/osx #{:user/unix}},
;     :descendants {:user/unix #{:user/osx}}}

(defmulti  compile-cmd  (juxt :os compiler))
(defmethod compile-cmd [::osx "gcc"] [m]
  (str "/usr/bin/" (get m :c-compiler)))
(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))
(compile-cmd osx)
;=> "/usr/bin/gcc"
(compile-cmd unix)
;=> "Unsure where to locate cc"



#_(
user=> (doc parents)
-------------------------
clojure.core/parents
([tag] [h tag])
  Returns the immediate parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy
nil
user=> (doc ancestors)
-------------------------
clojure.core/ancestors
([tag] [h tag])
  Returns the immediate and indirect parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy
nil
user=> (doc descendants)
-------------------------
clojure.core/descendants
([tag] [h tag])
  Returns the immediate and indirect children of tag, through a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy. Note: does not work on Java type inheritance
  relationships.
nil
user=> (doc isa?)
-------------------------
clojure.core/isa?
([child parent] [h child parent])
  Returns true if (= child parent), or child is directly or indirectly derived from
  parent, either via a Java type inheritance relationship or a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy
nil
user=> (doc make-hierarchy)
-------------------------
clojure.core/make-hierarchy
([])
  Creates a hierarchy object for use with derive, isa? etc.
nil
user=> (doc juxt)
-------------------------
clojure.core/juxt
([f] [f g] [f g h] [f g h & fs])
  Alpha - name subject to change.
  Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]
nil
)

; Juxt takes a bunch of functions and composes them into
; a function returning a vector of its argument(s)
; applied to each given function:
(def each-math (juxt + * - /))
(each-math 2 3)
;=> [5 6 -1 2/3]
((juxt take drop) 3 (range 9))
[(0 1 2) (3 4 5 6 7 8)]

; 9.3 - Types, protocols, and records

#_(
user=> (doc defrecord)
-------------------------
clojure.core/defrecord
([name [& fields] & opts+specs])
Macro
  Alpha - subject to change
  
  (defrecord name [fields*]  options* specs*)
  
  Currently there are no options.

  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:

  protocol-or-interface-or-Object
  (methodName [args*] body)*

  Dynamically generates compiled bytecode for class with the given
  name, in a package with the same name as the current namespace, the
  given fields, and, optionally, methods for protocols and/or
  interfaces.

  The class will have the (immutable) fields named by
  fields, which can have type hints. Protocols/interfaces and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols/interfaces.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directy.

  Method definitions take the form:

  (methodname [args*] body)

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  Methods should be supplied for all methods of the desired
  protocol(s) and interface(s). You can also define overrides for
  methods of Object. Note that a parameter must be supplied to
  correspond to the target object ('this' in Java parlance). Thus
  methods for interfaces will take one more argument than do the
  interface declarations. Note also that recur calls to the method
  head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.

  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).

  The class will have implementations of several (clojure.lang)
  interfaces generated automatically: IObj (metadata support) and
  IPersistentMap, and all of their superinterfaces.

  In addition, defrecord will define type-and-value-based equality and
  hashCode.

  When AOT compiling, generates compiled bytecode for a class with the
  given name (a symbol), prepends the current ns as the package, and
  writes the .class file to the *compile-path* directory.

  Two constructors will be defined, one taking the designated fields
  followed by a metadata map (nil for none) and an extension field
  map (nil for none), and one taking only the fields (using nil for
  meta and extension fields).
nil
user=> (doc read-string)
-------------------------
clojure.core/read-string
([s])
  Reads one object from the string s
nil
user=> (doc dissoc)
-------------------------
clojure.core/dissoc
([map] [map key] [map key & ks])
  dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s).
nil
user=> 
)


(defrecord TreeNode [val l r])
(TreeNode. 5 nil nil)
;=> #:user.TreeNode{"val 5, :l nil, :r nil}
(defn xconj [t v]
  (cond
    (nil? t)       (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else          (TreeNode. (:val t) (:l t) (xconj (:r t) v))))
(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))
(defn sample-tree (reduce xconj nil [3 5 2 4 6]))
(xseq sample-tree)
;=> (2 3 4 5 6)

; 9.3.2 - Protocols
; A protocol in Clojure is simply a set of function signatures, 
; each with at least one parameter, that are given a collective
; name.

(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop  [fixo])
  (fixo-peek [fixo]))

; Here we have created just the formal definition of
; the protocol. Protocols are implemented using
; one of the extend forms: extend, extend-type of
; extend-protocol.

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value)))
(xseq (fixo-push sample-tree 5/2))
;=> (2 5/2 3 4 5 6)
(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value)))
(fixo-push [2 3 4 5 6] 5/2)
;=> [2 3 4 5 6 5/2]

; Quote: 
; Clojure polymorphism lives in the protocol functions,
; not in the classes.

#_(
; user=> (doc defprotocol)
; -------------------------
; clojure.core/defprotocol
; ([name & opts+sigs])
; Macro
;   A protocol is a named set of named methods and their signatures:
;   (defprotocol AProtocolName
; 
;     ;optional doc string
;     "A doc string for AProtocol abstraction"
; 
;   ;method signatures
;     (bar [this a b] "bar docs")
;     (baz [this a] [this a b] [this a b c] "baz docs"))
; 
;   No implementations are provided. Docs can be specified for the
;   protocol overall and for each method. The above yields a set of
;   polymorphic functions and a protocol object. All are
;   namespace-qualified by the ns enclosing the definition The resulting
;   functions dispatch on the type of their first argument, which is
;   required and corresponds to the implicit target object ('this' in 
;   Java parlance). defprotocol is dynamic, has no special compile-time 
;   effect, and defines no new types or classes. Implementations of 
;   the protocol methods can be provided using extend.
; 
;   defprotocol will automatically generate a corresponding interface,
;   with the same name as the protocol, i.e. given a protocol:
;   my.ns/Protocol, an interface: my.ns.Protocol. The interface will
;   have methods corresponding to the protocol functions, and the
;   protocol will automatically work with instances of the interface.
; 
;   Note that you should not use this interface with deftype or
;   reify, as they support the protocol directly:
; 
;   (defprotocol P 
;     (foo [this]) 
;     (bar-me [this] [this y]))
; 
;   (deftype Foo [a b c] 
;    P
;     (foo [this] a)
;     (bar-me [this] b)
;     (bar-me [this y] (+ c y)))
;   
;   (bar-me (Foo. 1 2 3) 42)
;   => 45
; 
;   (foo 
;     (let [x 42]
;       (reify P 
;         (foo [this] 17)
;         (bar-me [this] x)
;         (bar-me [this y] x))))
;   => 17
; nil
; user=> 
; user=> (doc extend)
; -------------------------
; clojure.core/extend
; ([atype & proto+mmaps])
;   Implementations of protocol methods can be provided using the extend construct:
; 
;   (extend AType
;     AProtocol
;      {:foo an-existing-fn
;       :bar (fn [a b] ...)
;       :baz (fn ([a]...) ([a b] ...)...)}
;     BProtocol 
;       {...} 
;     ...)
;  
;   extend takes a type/class (or interface, see below), and one or more
;   protocol + method map pairs. It will extend the polymorphism of the
;   protocol's methods to call the supplied methods when an AType is
;   provided as the first argument. 
; 
;   Method maps are maps of the keyword-ized method names to ordinary
;   fns. This facilitates easy reuse of existing fns and fn maps, for
;   code reuse/mixins without derivation or composition. You can extend
;   an interface to a protocol. This is primarily to facilitate interop
;   with the host (e.g. Java) but opens the door to incidental multiple
;   inheritance of implementation since a class can inherit from more
;   than one interface, both of which extend the protocol. It is TBD how
;   to specify which impl to use. You can extend a protocol on nil.
; 
;   If you are supplying the definitions explicitly (i.e. not reusing
;   exsting functions or mixin maps), you may find it more convenient to
;   use the extend-type or extend-protocol macros.
; 
;   Note that multiple independent extend clauses can exist for the same
;   type, not all protocols need be defined in a single extend call.
; 
;   See also:
;   extends?, satisfies?, extenders
; nil
; user=> 
)

; ---
