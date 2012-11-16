;; EULER #005
;; ==========
;; 2520 is the smallest number that can be divided by each
;; of the numbers from 1 to 10 without any remainder.
;;
;; What is the smallest positive number that is evenly
;; divisible by all of the numbers from 1 to 20?
;;

(ns euler005
  (:use [util.primes]))

(defn solve [n]
  (reduce lcm (range 1 (inc n))))

(time (solve 20))
