(ns util.partition
  (:use [util.misc]))

(defn partitions 
  ([n] (partitions n n)) 
  ([n limit]
    (if (zero? n)
      [[]]
      (for [x (range (min n limit) 0 -1)
            p (partitions (- n x) x)]
        (cons x p)))))

(defn count-permutations [xs]
  (quot
    (factorial (count xs))
    (->>
      (frequencies xs)     
      vals
      (map factorial) 
      (reduce *))))
