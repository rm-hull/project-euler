;; EULER #123
;; ==========
;; Let p[n] be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the 
;; remainder when (p[n]-1)^n + (p[n]+1)^n is divided by p[n]^2.
;;
;; For example, when n = 3, p[3] = 5, and 4^3 + 6^3 = 280 == 5 mod 25.
;;
;; The least value of n for which the remainder first exceeds 10^9 is 7037.
;;
;; Find the least value of n for which the remainder first exceeds 10^10.
;;

(ns euler123
  (:use [util.primes]
        [util.misc]))

(defn remainder [[^long n ^long a]]
  (if (even? n)
    2
    (mod (* 2 n a) (* a a))))

(defn solve []
  (->> (map vector integers primes)
       (filter #(odd? (first %)))
       (map #(vector (first %) (remainder %)))
       (drop-while #(< (second %) 1e10))
       first))

(time (solve))
