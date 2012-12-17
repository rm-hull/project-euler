;; EULER #088
;; ==========
;; A natural number, N, that can be written as the sum and product of a 
;; given set of at least two natural numbers, {a1, a2, ... , ak} is called 
;; a product-sum number: N = a1 + a2 + ... + ak = a1 x a2 x ... x ak.
;;
;; For example, 6 = 1 + 2 + 3 = 1 x 2 x 3.
;;
;; For a given set of size, k, we shall call the smallest N with this 
;; property a minimal product-sum number. The minimal product-sum numbers 
;; for sets of size, k = 2, 3, 4, 5, and 6 are as follows.
;;
;;    k=2: 4  = 2 x 2                 = 2 + 2
;;    k=3: 6  = 1 x 2 x 3             = 1 + 2 + 3
;;    k=4: 8  = 1 x 1 x 2 x 4         = 1 + 1 + 2 + 4
;;    k=5: 8  = 1 x 1 x 2 x 2 x 2     = 1 + 1 + 2 + 2 + 2
;;    k=6: 12 = 1 x 1 x 1 x 1 x 2 x 6 = 1 + 1 + 1 + 1 + 2 + 6
;;
;; Hence for 2 <= k <= 6, the sum of all the minimal product-sum numbers is
;; 4 + 6 + 8 + 12 = 30; note that 8 is only counted once in the sum.
;; 
;; In fact, as the complete set of minimal product-sum numbers for 
;; 2 <= k <= 12 is {4, 6, 8, 12, 15, 16}, the sum is 61.
;;
;; What is the sum of all the minimal product-sum numbers for 
;; 2 <= k <= 12000?
;;

;            +  * 
; 11122   =  7  4
; 11123   =  8  6
; 11124   =  9  8
; 11125   = 10 10

; 11222   =  8  8
; 12222   =  9 16

; 221111  =  8  4
; 321111  =  9  6
; 421111  = 10  8
; 521111  = 11 10
; 621111  = 12 12

; 222111  =  9  8
; 322111  = 10 12

; 2211111 =  9  4
; 3211111 = 10  6
; 4211111 = 11  8
; 5211111 = 12 10
; 6211111 = 13 12
; 7211111 = 14 14

; 2221111 = 10  6
; 3221111 = 11 12

 
(ns euler088
  (:use [util.primes]
        [util.combinatorics]
        [util.misc]))

(prime-factors-of 60)
(prime-factors-of 60)

(reductions * (prime-factors-of 60))

(divisors 12)

(prime-factors-of 8)

(->> (divisors 12)
     (filter #(> % 1))
     (filter #(< % 12))
     (map #(quot 12 %)))

1,2,3,4


; 60 = 2 x 30
;    = 3 x 20
;    = 5 x 12

; 30 = 2 x 15
;    = 3 x 10
;    = 5 x 6

; 12 = 2 x 6
;    = 2 x 2 x 3 
;    = 3 x 4
;    = 3 x 2 x 2

; 6  = 2 x 3
;    = 3 x 2

(for [x '(2 2 3)
      y '(2 2 3)]
  [x y])

(combinations '(2 3 5) 3)
