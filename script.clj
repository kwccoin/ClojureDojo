(use 'pl.danieljanus.tagsoup)
(def foo (parse "http://www.example.com"))
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
