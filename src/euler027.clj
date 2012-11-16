;; EULER #027
;; ==========
;; Euler published the remarkable quadratic formula:
;;
;;    n² + n + 41
;;
;; It turns out that the formula will produce 40 primes for the consecutive
;; values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41
;; is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
;; divisible by 41.
;;
;; Using computers, the incredible formula  n² - 79n + 1601 was discovered,
;; which produces 80 primes for the consecutive values n = 0 to 79. The
;; product of the coefficients, -79 and 1601, is -126479.
;;
;; Considering quadratics of the form:
;;
;;    n² + an + b, where |a| < 1000 and |b| < 1000
;;
;; where |n| is the modulus/absolute value of n
;; e.g. |11| = 11 and |4| = 4
;;
;; Find the product of the coefficients, a and b, for the quadratic expression
;; that produces the maximum number of primes for consecutive values of n,
;; starting with n = 0.
;;
 
(ns euler027
  (:use [util.misc]
        [util.primes]))
 
(defn quadratic-equation [a b]
  (letfn [(f [n] (+ (* n n) (* a n) b))]
    (map f (iterate inc 0))))
 
(defn calc-max []
  (letfn [(max-count [x y] (if (> (:count x) (:count y)) x y))
          (num-primes [a b] (count (take-while is-prime? (quadratic-equation a b))))]
    (reduce max-count
      (for [a (range -999 1000)
            b (take-while #(< % 1000) primes)]
            { :coeffs [a b] :count (num-primes a b) }))))
 
(defn solve []
  (reduce * (:coeffs (calc-max))))
 
(time (solve))

