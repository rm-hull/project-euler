;; EULER #050
;; ==========
;; The prime 41, can be written as the sum of six consecutive primes:
;;
;;    41 = 2 + 3 + 5 + 7 + 11 + 13
;;
;; This is the longest sum of consecutive primes that adds to a prime
;; below one-hundred.
;;
;; The longest sum of consecutive primes below one-thousand that adds
;; to a prime, contains 21 terms, and is equal to 953.
;; 
;; Which prime, below one-million, can be written as the sum of the most
;; consecutive primes?
;;

(ns euler050
  (:use [util.primes]
        [util.misc]))

(defn prime-sums [xs]
  (letfn [(accum-fn [[a1 b1] [a2 b2]] (vector (max a1 a2) (+ b1 b2)))]
    (filter #(is-prime? (second %))
            (reductions accum-fn (map vector integers xs)))))

(defn solve []
  (reduce (fn [a b] (if (> (first a) (first b)) a b))
    (for [i (range 1 500)]
      (last 
        (take-while #(< (second %) 1000000) 
                    (prime-sums (drop i primes)))))))

(time (solve))
