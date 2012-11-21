;; EULER #076
;; ==========
;; It is possible to write five as a sum in exactly six different ways:
;;
;;  4 + 1
;;  3 + 2
;;  3 + 1 + 1
;;  2 + 2 + 1
;;  2 + 1 + 1 + 1
;;  1 + 1 + 1 + 1 + 1
;;
;; How many different ways can one hundred be written as a sum of at 
;; least two positive integers?
;;

(ns euler076)

(defn count-partitions [c ^long v]
  (let [f (first c)]
    (if (= f 1) 
      1
      (reduce + 
        (for [n (range 0 (inc (quot v f)))]
          (count-partitions (rest c) (- v (* n f))))))))

(defn solve [n]
  (count-partitions (range (dec n) 0 -1) n))

(time (solve 100))
