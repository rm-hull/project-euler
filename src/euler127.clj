;; EULER #127
;; ==========
;; The radical of n, rad(n), is the product of distinct prime factors of n.
;; For example, 504 = 2^3 x 3^2 x 7, so rad(504) = 2 x 3 x 7 = 42.
;;
;; We shall define the triplet of positive integers (a, b, c) to be an 
;; abc-hit if:
;;
;;    GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
;;    a < b
;;    a + b = c
;;    rad(abc) < c
;;    
;; For example, (5, 27, 32) is an abc-hit, because:
;;
;;    GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
;;    5 < 27
;;    5 + 27 = 32
;;    rad(4320) = 30 < 32
;;    
;; It turns out that abc-hits are quite rare and there are only thirty-one
;; abc-hits for c < 1000, with sum(c) = 12523.
;;
;; Find sum(c) for c < 120000.
;;
;; Note: This problem has been changed recently, please check that you are
;; using the right parameters.
;;

(ns euler127
  (:use [util.primes]))

(defn radical-of [n]
  (reduce * (distinct (prime-factors-of n))))

(defn is-abc-hit? [a b c]
  (and
    (< a b)
    (= 1 (gcd a b))
    (= 1 (gcd a c))
    (= 1 (gcd b c))
    (< (radical-of (* a b c)) c)
  ))

(defn solve [n]
  (reduce +
    (for [c (range 1 n)
          b (range (inc (quot c 2)) c)
          :let [a (- c b)]
          :when (is-abc-hit? a b c)]
      c)))

(time (solve 1000))
