;> user> (defn consecutive [s] (->> s (partition-by identity) (reduce #(+
;> % (dec (count %2))) 0)))
;> #'user/consecutive
;> user> (consecutive "abcdefg")
;> 0
;> user> (consecutive "aabcdddefg")
;> 3

(defn consecutive [s]
  (->>
       s
       (partition-by identity)
       (reduce
          #(+ %1 (dec (count %2)))
          0)))

; (reduce (partition-by identity s))
; reduce-f:    #(+ %1 (dec (count %2)))
; reduce-val:  0
; reduce-coll: (partition-by identity s)
; result: (f val (first coll)) ... then to second...

; partition-by
; clojure.core
; 
;     (partition-by f coll)
; 
; Applies f to each value in coll, splitting it each time f returns
; a new value. Returns a lazy seq of partitions.
; 
; 
; reduce
; clojure.core
; 
;     (reduce f coll)
;     (reduce f val coll)
; 
; f should be a function of 2 arguments.
; 
; - If val is not supplied, returns the result of applying f to
; the first 2 items in coll, then applying f to that result and the 3rd
; item, etc.
; - If coll contains no items, f must accept no arguments as well, and
; reduce returns the
; result of calling f with no arguments.
; - If coll has only 1 item, it is returned and f is not called.
; - If val is supplied, returns the result of applying f to val and
; the first item in coll, then applying f to that result and the 2nd item, etc.
; - If coll contains no items, returns val and f is not called.
; 

;#'user/consecutive
;user> (consecutive "abcdefg")
;0

;user> (consecutive "aa b c ddd e f g")
;3

(reduce  (f 0 '(aa)))
(reduce  (f 1 '(b)))
(reduce  (f 1 '(c)))
(reduce  (f 1 '(ddd))) = (+ 1 2) = 3
