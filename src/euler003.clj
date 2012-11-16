;; EULER #003
;; ==========
;; Find the largest prime factor of a composite number.
;; 
;; The prime factors of 13195 are 5, 7, 13 and 29.
;; 
;; What is the largest prime factor of the number 600,851,475,143 ?
;;

(ns euler003
  (:use [util.primes]))

(prime-factors-of 13195)

(time (sort (prime-factors-of 600851475143)))
