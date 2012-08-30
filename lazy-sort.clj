(ns joy.q)

(defn nom [n] (take n (repeatedly #(rand-int n))))

(defn sort-parts
  "Lazy, tail-recursive, incremental quicksort. Works against
   and creates partitions based on the pivot, defined as 'work'."
  [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list* 
                  (filter smaller? xs)
                  pivot
                  (remove smaller? xs)
                  parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

#_(

list*
clojure.core

    (list* args)
    (list* a args)
    (list* a b args)
    (list* a b c args)
    (list* a b c d & more)

Creates a new list containing the items prepended to the rest, the
last of which will be treated as a sequence
)
