

; Just a toy.
; create a map with a string as key and a random number as value.

; lol try: (filter #(re-find #"Case" %) (map #(.getName %) (seq (.getMethods String))))

;(defn n-rand [n] (take n (repeatedly rand)))
; (map #(.toLowerCase %) (keyz))

;every 2-combination of letters
;(for [x (keyz) y (keyz)] (map str x y)) 

;apply a prefix
;(doseq [x ":" y (keyz)] (println (str x y)))

(use '[clojure.contrib.math :only (round)])
(defn keyz "create the keyz" [] (map str (map char (range 65 91))))
(defn foo [x] 
  (loop [times (dec x)
         k (keyz)
         s (keyz)]
    (if (zero? times)
      s
      (recur (dec times) k (for [x (keyz) y s] (map str (str x) y)))
)))
(defn rand-map
    ([] (rand-map 10)) 
    ([size] (let [
      h size  
      length (inc (round (/ h 26)))
      v (take h (repeatedly rand))
      k (map str (map first (take h (foo length))))] 
      (zipmap k v))
))

;;;;

(for [x (range) :while (< x 10) 
      y (range) :while (<= y x)]
  [x y])

=>(
[0 0]
[1 0] [1 1]
[2 0] [2 1] [2 2]
[3 0] [3 1] [3 2] [3 3]
[4 0] [4 1] [4 2] [4 3] [4 4]
[5 0] [5 1] [5 2] [5 3] [5 4] [5 5]
[6 0] [6 1] [6 2] [6 3] [6 4] [6 5] [6 6]
[7 0] [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7]
[8 0] [8 1] [8 2] [8 3] [8 4] [8 5] [8 6] [8 7] [8 8]
[9 0] [9 1] [9 2] [9 3] [9 4] [9 5] [9 6] [9 7] [9 8] [9 9])

; compose and create a function, based on its arguments
(defn fnth [n]
  (apply comp
    (cons first
      (take (dec n) (repeat rest)))))

; create some keywords...
(map (comp keyword #(.toLowerCase %) name) '(a B C))

#_(

cons
clojure.core

    (cons x seq)

Returns a new seq where x is the first element and seq is
the rest.


keyword
clojure.core

    (keyword name)
    (keyword ns name)

Returns a Keyword with the given namespace and name. Do not use :
in the keyword strings, it will be added automatically.


repeat
clojure.core

    (repeat x)
    (repeat n x)

Returns a lazy (infinite!, or length n if supplied) sequence of xs.


name
clojure.core

    (name x)

Returns the name String of a string, symbol or keyword.
)
