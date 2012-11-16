;; EULER #006
;; ==========
;; The sum of the squares of the first ten natural numbers is,
;;
;; 1^2 + 2^2 + ... + 10^2 = 385
;;
;; The square of the sum of the first ten natural numbers is,
;;
;; (1 + 2 + ... + 10)^2 = 552 = 3025
;;
;; Hence the difference between the sum of the squares of the 
;; first ten natural numbers and the square of the sum is 
;; 3025 - 385 = 2640.
;;
;; Find the difference between the sum of the squares of the
;; first one hundred natural numbers and the square of the sum.
;;

(ns euler006
  (:use [util.misc]))

(defn square-of-sums [n] 
  (iexpt (triangle n) 2))

(defn sum-of-squares [n]
  (quot 
    (*
      n
      (inc n)
      (inc (+ n n)))
    6))

(defn solve [n]
  (- (square-of-sums n) (sum-of-squares n)))

(time (solve 100))
