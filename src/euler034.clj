;; EULER #034
;; ==========
;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;;
;; Find the sum of all numbers which are equal to the sum of the
;; factorial of their digits.
;;
;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.
;;

(ns euler034
  (:use [util.misc]))

(def fac-digits
  (->> (take 10 factorial-seq)
       (map int)
       vec))

(defn is-factorion? [n]
  (= n (sum-of fac-digits n)))

(defn solve []
  (let [upper-bound (* 7 (factorial 9))]
    (reduce + (filter is-factorion? (range 3 upper-bound)))))

(time (solve))
