((fn duper [sequ]
  (let [s (seq sequ)]
  (lazy-seq 
    (loop [f (first s) 
           r (rest s)]
      '(f f)
      (if (not (empty? r)) (recur (first r)(rest r)))))))
[1 2 3])


((fn duper [sequ]
  (flatten (for [e (seq sequ)] (list e e))))
[1 2 3])

((fn duper [sequ]
  (for [e (seq sequ)] (list e e)))
[1 2 3])

(
(fn duper [sequ]
  (let [s (seq sequ)]
    (lazy-seq
      (loop [f (first s)    
             r (rest s)
             result (list)] 
        ;(println "f: "f" r: "r" result: "result)
        (if (empty? r) 
          (reverse (cons (list f f) result))
          (recur 
            (first r)
            (rest r)
            (cons (list f f) result)))))))
[1 2 3 4])

;;;;
; 32. Duplicate a Sequence
; 
; Difficulty:	Easy
; Topics:	seqs
; 
; Write a function which duplicates each element of a sequence.
; (= (__ [1 2 3]) '(1 1 2 2 3 3))
; (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))

; Solutions:
(fn [coll]
  (apply concat (map #(repeat 2 %) coll)))

;austintaylor's solution:
#(interleave % %)

;maximental's solution:
mapcat #(list % %)

;nikelandjelo's solution:
(fn [v] (reduce #(conj %1 %2 %2) [] v))

;norman's solution:
(fn dupl [items]
  (if (empty? items)
    ()
    (let [f (first items) r (rest items)]
      (concat [f f] (dupl r)))))
;;;

(
(fn duper [sequ]
  (let [s (seq sequ)]
    (lazy-seq
      (loop [f (first s)  
             r (rest s)
             result (list)] 
        (println "f: "f" r: "r" result: "result)
        (if (empty? r)  
          (reverse (concat (repeat 2 f) result))
          (recur 
            (first r)
            (rest r)
            (concat (repeat 2 f) result)))))))
[1 2 3])
