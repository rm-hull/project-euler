;; EULER #120
;; ==========
;; Let r be the remainder when (a-1)^n + (a+1)^n is divided by a^2.
;;
;; For example, if a = 7 and n = 3, then r = 42: 
;; 6^3 + 8^3 = 728 = 42 mod 49. And as n varies, so too will r, 
;; but for a = 7 it turns out that r_max = 42.
;;
;; For 3 <= a <= 1000, find  sum(r_max).
;;

(ns euler120
  (:use [util.misc]))

(defn r-max [^long a]
  (- (* a a)
     (if (even? a)
       (+ a a)
       a)))

(defn solve []
  (->> (range 3 1001)
       (map r-max)
       (reduce +)))

(time (solve))
