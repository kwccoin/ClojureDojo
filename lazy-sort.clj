;The lazy qsort will be able to gather the first element beacause it only takes some small subset of comparisons to gather the numbers into left-side smaller and right-side larger partitions and sort those smaller pieces only.

;The benefits?
;You can take sorted portions of a large sequence without having to pay the cost of sorting its entirety.

(ns joy.q)
(defn nom [n] (take n (repeatedly #(rand-int n))))

(defn sort-parts
  "Lazy, tail-recursive, incremental quicksort. Works against
   and creates partitions based on the pivot, defined as 'work'."
  [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        ; if part contains elements then:
        (let [smaller? #(< % pivot)]
          (println "[pivot:" pivot " xs: " xs 
            " parts: " parts "]")
          (recur (list* 
                  (filter smaller? xs)
                  pivot
                  (remove smaller? xs)
                  parts)))
        ; when in xs there are not element smaller the pivot
        ; return the most recent pivot and the part 
        ; became the new work.
        ; else:
        (when-let [[x & parts] parts]
          (println "x: " x " parts: " parts)
          (cons x (sort-parts parts)))))))

; work is always a list that alternates between lazy
; seqs and pivots.

; the algorithm start working with a list created from a seq.
; Example:
; (qsort [4 3 1 2])
; [4 3 1 2] -> (list [4 3 1 2]) -> ([4 3 1 2])
; thus, in the first step we have:
; work  = ([4 3 1 2])
; part  = [4 3 1 2]
; parts = nil
; pivot = 4
(defn qsort [xs]
  (sort-parts (list xs)))

#_(
;example:

(qsort [8 5 6 1 3 2])
(
[pivot: 8  xs:  (5 6 1 3 2)  parts:  nil ]
[pivot: 5  xs:  (6 1 3 2)  parts:  (8 ()) ]
[pivot: 1  xs:  (3 2)  parts:  (5 (6) 8 ()) ]
x:  1  parts:  ((3 2) 5 (6) 8 ())
[pivot: 3  xs:  (2)  parts:  (5 (6) 8 ()) ]
[pivot: 2  xs:  nil  parts:  (3 () 5 (6) 8 ()) ]
x:  2  parts:  (() 3 () 5 (6) 8 ())
1 x:  3  parts:  (() 5 (6) 8 ())
2 x:  5  parts:  ((6) 8 ())
3 [pivot: 6  xs:  nil  parts:  (8 ()) ]
x:  6  parts:  (() 8 ())
5 x:  8  parts:  (())
6 8)

; ---

(qsort [9 2 5 4 3 1 7])
(
[pivot: 9  xs:  (2 5 4 3 1 7)  parts:  nil ]
[pivot: 2  xs:  (5 4 3 1 7)  parts:  (9 ()) ]
[pivot: 1  xs:  nil  parts:  (2 (5 4 3 7) 9 ()) ]
x:  1  parts:  (() 2 (5 4 3 7) 9 ())
x:  2  parts:  ((5 4 3 7) 9 ())
1 [pivot: 5  xs:  (4 3 7)  parts:  (9 ()) ]
[pivot: 4  xs:  (3)  parts:  (5 (7) 9 ()) ]
[pivot: 3  xs:  nil  parts:  (4 () 5 (7) 9 ()) ]
x:  3  parts:  (() 4 () 5 (7) 9 ())
2 x:  4  parts:  (() 5 (7) 9 ())
3 x:  5  parts:  ((7) 9 ())
4 [pivot: 7  xs:  nil  parts:  (9 ()) ]
x:  7  parts:  (() 9 ())
5 x:  9  parts:  (())
7 9)

)

; user=> (let [[part & parts](list [4 3 1 2])] (if-let [[pivot & xs] (seq part)] (println "part: " part " parts: " parts " pivot: " pivot " xs: " xs) (print "damn")))
; => part:  [4 3 1 2]  parts:  nil  pivot:  4 xs: (3 1 2) 
; => nil

#_(

list*
clojure.core

    (list* args)
    (list* a args)
    (list* a b args)
    (list* a b c args)
    (list* a b c d & more)

Creates a new list containing the items prepended to the rest, the
last of which will be treated as a sequence
)

; user=> (list* '(3 1 2) 4 '(9 6))
; => ((3 1 2) 4 9 6)


