

; Just a toy.
; create a map with a string as key and a random number as value.

; lol try: (filter #(re-find #"Case" %) (map #(.getName %) (seq (.getMethods String))))

; Create a list of string, length 1
(defn keyz "create the keyz" [] (map str (map char (range 65 91))))
; (map #(.toLowerCase %) (keyz))

(defn n-rand [n] (take n (repeatedly rand)))

#_(
(use '[clojure.contrib.math :only (round)])
(let [h 10  
      l (inc (round (/ h 26)))
      v (take h (repeatedly rand))
      k (take h (keyz))] 
      (zipmap k v)
))

(defn foo [x] 
  (loop [times (dec x)
         k (keyz)
         s (keyz)]
    (if (zero? times)
      s
      (recur (dec times) k (for [x (keyz) y s] (map str x y)))
)))

(foo 3)

;every 2-combination of letters
;(for [x (keyz) y (keyz)] (map str x y)) 

;apply a prefix
;(doseq [x ":" y (keyz)] (println (str x y)))



