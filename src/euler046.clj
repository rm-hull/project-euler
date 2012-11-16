;; EULER #046
;; ==========
;; It was proposed by Christian Goldbach that every odd composite number
;; can be written as the sum of a prime and twice a square.
;;
;;    9 = 7 + 2x1^2
;;    15 = 7 + 2x2^2
;;    21 = 3 + 2x3^2
;;    25 = 7 + 2x3^2
;;    27 = 19 + 2x2^2
;;    33 = 31 + 2x1^2
;;
;; It turns out that the conjecture was false.
;;
;; What is the smallest odd composite that cannot be written as the sum 
;; of a prime and twice a square?
;;

(ns euler046
  (:use [util.primes]
        [util.misc]))

(defn is-goldbach-conjecture? [n]
  (let [double (partial * 2)]
    (not-empty
      (for [p  (take-while #(<= % n) primes)
            sq (take-while #(<= % (- n p)) (map double squares))
            :when (= n (+ p sq))]
        [p sq]))))

(defn solve []
  (first (remove is-goldbach-conjecture? (filter odd? composites))))

(time (solve))
