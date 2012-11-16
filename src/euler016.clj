;; EULER #016
;; ==========
;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;;
;; What is the sum of the digits of the number 2^1000?
;;

(ns euler016
  (:use [util.misc]))
 
(defn solve [pow]
  (->> (iexpt (bigint 2) pow)
       digits
       (reduce +)
       long))

(time (solve 1000))
