;34. Implement range
; 
; Difficulty:	Easy
; Topics:	seqs core-functions
; Special restrictions: range
;
; Write a function which creates a list of all integers in a given range.
; (= (__ 1 4) '(1 2 3))
; (= (__ -2 2) '(-2 -1 0 1))
; (= (__ 5 8) '(5 6 7))

(fn ranger [from to]
  (take (- to from) (iterate inc from)))

; norman's solution:
(fn [start end]
  (seq
   (loop [i start vals []]
     (if (= i end)
       vals
       (recur (inc i) (conj vals i))))))

(defn vrange2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

#_(
-------------------------
clojure.core/lazy-seq
([& body])
Macro
  Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls.
-------------------------
clojure.core/while
([test & body])
Macro
  Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil
-------------------------
clojure.core/iterate
([f x])
  Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects
)
