

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
      (recur (dec times) k (for [x (keyz) y s] (map str x y)))
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

