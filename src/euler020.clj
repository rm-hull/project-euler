;; EULER #020
;; ==========
;; n! means n x (n x 1) x ... x 3 x 2 x 1
;; For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
;; and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
;; Find the sum of the digits in the number 100!
;;

(ns euler020
  (:use [util.misc]))

(defn solve [n]
  (->> (nth factorial-seq n)
       str
       (map char-to-int)
       (reduce +)))
 
(time (solve 100))
