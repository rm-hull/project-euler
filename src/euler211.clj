;; EULER #211
;; ==========
;; For a positive integer n, let σ2(n) be the sum of the squares of its
;; divisors. For example,
;;
;;    σ2(10) = 1 + 4 + 25 + 100 = 130.
;;
;; Find the sum of all n, 0 < n < 64,000,000 such that σ2(n) is a perfect
;; square.
;;

(ns euler211
  (:use [util.misc]
        [util.primes]))

(->> squares
     (take-while #(< % 1000))
     (map prime-factors-of))

(->> (range 1 50000)
     (map #(vector % (sigma 2 %)))
     (filter #(is-square? (second %)))
     (map #(cons (sigma 0 (first %)) %)))

(* 5 12)

(->> (range 1 40)
     (map (partial sigma 0)))

42)

(divisors 287)

(+ 4 9 49)
