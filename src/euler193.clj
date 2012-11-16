;; EULER #193
;; ==========
;; A positive integer n is called squarefree, if no square of a prime divides
;; n, thus 1, 2, 3, 5, 6, 7, 10, 11 are squarefree, but not 4, 8, 9, 12.
;; 
;; How many squarefree numbers are there below 2^50?
;;

(ns euler193
  (:use [util.primes]
        [util.misc]))

(quadratfrei? 9)

(doseq [x (->> integers
               (map #(vector % (digits % 2) (quadratfrei? %)))
               (take 30))]
  (prn x))


