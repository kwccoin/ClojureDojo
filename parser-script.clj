; Let's try to parse a webpage using destructuring in a smart way.

; the library
(use 'pl.danieljanus.tagsoup)

; the page
(def foo (parse "http://www.example.com"))
(def clojure-com (parse "http://clojure.com"))

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
            { :more  more :href (property-map :href)}
            (map parz more)
          )
          ;(apply str src)
        )
      )
    )
  )
)
(remove nil? (flatten (parz foo)))
(remove nil? (flatten (parz clojure-com)))

; (map #(str "http://"%1 (%2 :href)) "www.example.com" rez)


