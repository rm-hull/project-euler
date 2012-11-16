;; EULER #148
;; ==========
;; We can easily verify that none of the entries in the first seven rows of
;; Pascal's triangle are divisible by 7:
;;
;;                      1
;;                   1     1
;;                1     2     1
;;             1     3     3     1
;;          1     4     6    4      1
;;       1     5    10    10    5      1
;;    1     6    15    20    15    6      1
;;
;; However, if we check the first one hundred rows, we will find that only
;; 2361 of the 5050 entries are not divisible by 7.
;;
;; Find the number of entries which are not divisible by 7 in the first one
;; billion (10^9) rows of Pascal's triangle.
;;

(ns euler148
  (:use [util.misc]))

(defn count-odd-rows [row base]
  (->> (digits row base)
       (reduce #(* %1 (inc %2)) 1)))

(defn solve [n base]
  (if (< n base)
    (triangle n)
    (let [q (quot n base)
          r (rem n base)]
      (+ 
        (* (count-odd-rows q base) (triangle r))
        (* (solve q base) (triangle base))))))

(time (solve 1e9 7))
