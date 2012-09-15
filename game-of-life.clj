;Conway's Game Of Life

; source:
; "Adventures In Declarative Programming: Conway's Game Of Life"
; article from "Adventures In Declarative Programming" by Manuel Rotter 
; http://programmablelife.blogspot.it/2012/08/conways-game-of-life-in-clojure.html
; 
; http://programmablelife.blogspot.co.at/2012/09/clojure-connect-four-1-checking-winner.html
; http://programmablelife.blogspot.it/2012/09/clojure-connect-four-1-checking-winner.html

; Conway's Game of Life
; This zero-player "game" basically consist of a cellular automaton following four rules which were devised by John Horton Conway. These rules together with the initial input determine the whole evolution of the cells in this game.
; The world in which the evolution of those cells takes place is an infinite two-dimensional grid. Each cell is either dead or alive and has exactly eight neighbours.
; The four rules are:
; 1. Any live cell with fewer than two live neighbours dies, as if caused by under-population.
; 2. Any live cell with two or three live neighbours lives on to the next generation.
; 3. Any live cell with more than three live neighbours dies, as if by overcrowding.
; 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
; The game starts by giving the world an initial state with certain cells being alive. Then those four rules are applied and by doing so, one of three things happen to each cell: Nothing, death or birth. After applying those rules the first time, we have the second generation of our world. The same procedure starts over and over again, until either we decide to stop it or all cells are dead.

(defn create-world
  "Creates a rectangular world with the specified width and height.
   Optionally takes coordinates of living cells."
  [width height & living-cells]
  (vec 
    (for [y (range w)]
      (vec 
        (for [x (range h)] 
          (if (contains? (first living-cells) [y x])
            "x" ; living cell
            " " ; dead/empty cell
          ))))))

(create-world 4 4)
;=> [
; [" " " " " " " "]
; [" " " " " " " "]
; [" " " " " " " "]
; [" " " " " " " "]]

(create-world 4 4 #{[0 0], [1 1], [2 2], [3 3]})
;=> [
; ["X" " " " " " "]
; [" " "X" " " " "]
; [" " " " "X" " "]
; [" " " " " " "X"]]

; Now, instead of writing a function which behaves exactly according 
; to the rules of Conway's Game of Life, we could write a high-order
; function that takes three arguments: A function to determine the
; neighbours, a predicate which determines whether a cell shall be
; given life (e.g. #{3} for Conway's rules) and one predicate that
; determines whether a cell survives to the next generation (#{2 3}).
; It then returns a function which calculates the next generation
; according to the given rules.

(defn neighbours
  "Determines all the neighbours of a given coordinate"
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] 
    :when (not= 0 dx dy)] ; avoid the check of the "starting/center/0-0" cell
    [(+ dx x) (+ dy y)]))

(neighbours [1 1])
; => ([0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2])

(defn stepper
  "Returns a step function for Life-like cell automata.
   neighbours takes a location and return a sequential collection
   of locations. survive? and birth? are predicates on the number
   of living neighbours."
  [neighbours birth? survive?]
  (fn [cells] ; cells contains a list of coordinates
				(set (for [
                    [location n] (frequencies (mapcat neighbours cells))
                    ; mapcat returns the concatanated list of the result 
                    ; of calling neighbours on all coordinates in cells.
                    ; frequencies then returns a hash table (map) of 
                    ; each distinct item returned by the mapcat and 
                    ; associates it with the number of occurence.
                    :when (if (cells location) (survive? n) (birth? n))
                    ; :when makes sure only those coordinates loc are
                    ; returned that pass the if test.
                    ; If loc is in cells and the number of living
                    ; neighbours n is high enough according to the
                    ; predicate survive?, the loc in returned.
                    ; If loc is not in cells, only those loc are returned
                    ; that have enough living neighbours n according
                    ; to the predicate birth? to give that cell life.
                  ]
                  loction))))

; the horizontal position of the blinker
(create-world 4 4 #{[1 0][1 1][1 2]})
((stepper neighbours #{3} #{2 3}) #{[1 0] [1 1] [1 2]})
; #{} is a set, which can be used as a predicate (#{3} and #{2 3})
; to check whether the arguments given to it are in that set.
; For example: (#{2 3} 3) => 3 and (#{2 3} 1) => nil

; (#{2 3} 3)
;=> 3
; (#{2 3} 1)
;=> nil

; so, stepper return the vertical position of the blinker
;=> #{[2 1] [1 1] [0 1]}

; Patterns
; with a set of vectors containing two integers each, which are the
; coordinates of the living cells.
(def glider #{[2 0] [2 1] [2 2] [1 2] [0 1]})
(def light-spaceship #{[2 0] [4 0] [1 1] [1 2] [1 3] [4 3] [1 4] [2 4] [3 4]})
 
; Stepper
(def conway-stepper (stepper neighbours #{3} #{2 3}))

; The final function
(defn conway
  "Generates world of given size with initial pattern 
    in specified generation"
  [[width height] pattern iterations]
    (->> (iterate conway-stepper pattern)
         (drop iterations)
         first
         (create-world width height)
         (map println)))

;test it out:
(conway [5 15] light-spaceship 0)
(conway [4 4] glider 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

set
clojure.core
(set coll)
Returns a set of the distinct elements of coll.

frequencies
clojure.core
(frequencies coll)
Returns a map from distinct items in coll to the number of times
they appear.

mapcat
clojure.core
(mapcat f & colls)
Returns the result of applying concat to the result of applying map
tverticalo f and colls. Thus function f should return a collection.

iterate
clojure.core
(iterate f x)
Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects

->>
clojure.core
(->> x form)
(->> x form & more)
Threads the expr through the forms. Inserts x as the
last item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
last item in second form, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns game-of-life.core)

(def neighbor-cords
  [[-1 -1]
   [0 -1]
   [1 -1]
   [-1 0]
   [1 0]
   [-1 1]
   [0 1]
   [1 1]])

(defn create-cell [alive x y]
  {:alive alive :x x :y y})

(defn cell-at [loc world]
  (let [[x y] loc]
    (get (get world y) x)))

(defn find-neighbors [cell world]
  (let [x (cell :x) y (cell :y)]
    (remove nil? (map
      (fn [cord]
        (cell-at [(+ x (first cord)) (+ y (second cord))]
                 world))
      neighbor-cords))))

(defn alive-next? [cell world]
  (let [total-neighbors
          (count
          (filter #(= true (% :alive)) (find-neighbors cell world)))]
    (or
      (and (not (cell :alive)) (= total-neighbors 3))
      (and (cell :alive) (or (= total-neighbors 2) (= total-neighbors 3))))))

(defn apply-to-world [f world]
  (let [rows (count world)
        cols (count (first world))]
    (vec (map
      (fn [row y]
        (vec (map
          (fn [_ x]
            (f x y))
          row (range cols))))
      world (range rows)))))

(defn next-generation [world]
  (apply-to-world
    (fn [x y]
      (create-cell (alive-next? (cell-at [x y] world) world) x y))
    world))

(defn new-world [width height]
  (let [world (map (fn [_] (range width)) (range height))]
    (apply-to-world
      (fn [x y]
        (create-cell false x y))
      world)))

