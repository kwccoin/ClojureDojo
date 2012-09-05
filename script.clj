; Let's try to parse a webpage using destructuring in a smart way.

; the library
(use 'pl.danieljanus.tagsoup)

; the page
(def foo (parse "http://www.example.com"))

; the destructure functions
(defn tri-vec [parsed-page] (let [[tag property-map & more :as all] parsed-page] [tag property-map (vec more) all]))
(defn tri-map [parsed-props] (let [[tag property-map & more :as all] parsed-props] {:tag tag :property-map property-map :more (vec more) :all all}))

; the parser function
(defn parz [src] 
  (let [ttt (tri-vec src)]
    (let [tag (ttt 0) property-map (ttt 1) more (ttt 2)] 
      (if more
        (if (keyword? tag )
          (if (= tag (keyword "a"))
            (property-map :href)
            (map parz more)
          )
          (apply str src)
        )
      )
    )
  )
)
(parz foo )
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
