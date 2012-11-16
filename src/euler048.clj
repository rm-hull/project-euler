;; EULER #048
;; ==========
;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
;;

(ns euler048
  (:use [util.misc]))

(defn solve [n]
  (let [mask (iexpt 10 10)]
    (mod (reduce + (map #(iexpt % %) (range 1N (inc n)))) mask)))

(time (solve 1000))
