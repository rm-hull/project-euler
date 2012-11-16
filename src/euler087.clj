;; EULER #087
;; ==========
;; The smallest number expressible as the sum of a prime square, prime 
;; cube, and prime fourth power is 28. In fact, there are exactly four 
;; numbers below fifty that can be expressed in such a way:
;;
;;    28 = 2^2 + 2^3 + 2^4
;;    33 = 3^2 + 2^3 + 2^4
;;    49 = 5^2 + 2^3 + 2^4
;;    47 = 2^2 + 3^3 + 2^4
;;
;; How many numbers below fifty million can be expressed as the sum of a 
;; prime square, prime cube, and prime fourth power?
;;

(ns euler087
  (:use [util.primes]))

(defn solve [^long limit]
  (let [squares (map #(* % %) primes)
        cubes   (map #(* % % %) primes)
        quads   (map #(* % % % %) primes)]
    (count (distinct
      (for [x (take-while #(<= % limit) squares)
            y (take-while #(<= % (- limit x)) cubes)
            z (take-while #(<= % (- limit x y)) quads)
            :let [s (+ x y z)]
            :when (< s limit)]
        s)))))

(time (solve 50000000))
