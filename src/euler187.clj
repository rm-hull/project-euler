;; EULER #187
;; ==========
;; A composite is a number containing at least two prime factors. For 
;; example, 15 = 3 x 5; 9 = 3 x 3; 12 = 2 x 2 x 3.
;; 
;; There are ten composites below thirty containing precisely two, not 
;; necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 
;; 26.
;; 
;; How many composite integers, n < 10^8, have precisely two, not 
;; necessarily distinct, prime factors?
;;

;; semiprime - http://oeis.org/A066265

(ns euler187
  (:use [util.misc] 
        [util.primes]))

(defn solve [^long n]
  (let [search-space (quot n 2)]
    (count
      (for [^long x (primes-range 2 search-space)
            ^long y (primes-range x (quot n x))
            :when (<= (* x y) n)]
        1))))

(defn count-primes [n]
  (count (primes-range 2 n)))

(triangle-root (count-primes 30))

(time (solve 1E8))
;
;            1 1 1 1 2
;    2 3 5 7 1 3 7 9 3
;   2. . . . . . . . .
;   3  . . . . . 
;   5    . . 
;   7      .
;  11
;  13

(* 3 17)

6 3 1
8 5 2 1

(defn solve2 [^long n]
  (loop [n    (count (primes-range 2 n))
         accu n]
    (if (= n 1)
      (+ n accu)
      (recur (quot n 2) )
