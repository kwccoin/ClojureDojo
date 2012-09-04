(use 'pl.danieljanus.tagsoup)
(def foo (parse "http://www.example.com"))
(defn tri [t] (let [[a b & c] t] [a b (vec c)]))
(defn parz [src] 
  (let [ttt (tri src)]
    (if (ttt 2)
      (if (keyword? (ttt 0))
        (map parz (ttt 2))
        (if (= (ttt 0) ":a")
          (ttt 1) 
          (apply str src)
        )
      )
    )
  )
)
(defn clea [tst]
  (let [[h p & o] tst,
        c (count o)
       ]
    ;(println " ---> h:" h "| p:" p "| c:" c )
    ;(if (and h (= h :a)) (println (p :href)))
    (if o 
      (if (keyword? h)
        (map clea o)
        (apply str tst)
      )
    )
  )
)
(clea foo)
