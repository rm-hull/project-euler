;; EULER #104
;; ==========
;; The Fibonacci sequence is defined by the recurrence relation:
;;
;;    F[n] = F[n-1] + F[n-2], where F[1] = 1 and F[2] = 1.
;;
;; It turns out that F[541], which contains 113 digits, is the first Fibonacci
;; number for which the last nine digits are 1-9 pandigital (contain all the 
;; digits 1 to 9, but not necessarily in order). And F[2749], which contains 
;; 575 digits, is the first Fibonacci number for which the first nine digits 
;; are 1-9 pandigital.
;;
;; Given that F[k] is the first Fibonacci number for which the first nine
;; digits AND the last nine digits are 1-9 pandigital, find k.
;;

(ns euler104
  (:use [util.fibonacci]
        [util.misc]
        [util.primes]))

(bigint (+ (/ (iexpt (bigdec golden-ratio) 541) (Math/sqrt 5)) 1/2))

(nth fib-seq 540)
(nth fib-seq 2748)

(prime-factors-of 542)
