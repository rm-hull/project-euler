;; EULER #047
;; ==========
;; The first two consecutive numbers to have two distinct prime 
;; factors are:
;; 
;;    14 = 2 x 7
;;    15 = 3 x 5
;;
;; The first three consecutive numbers to have three distinct 
;; prime factors are:
;;
;;    644 = 2² x 7 x 23
;;    645 = 3 x 5 x 43
;;    646 = 2 x 17 x 19.
;;
;; Find the first four consecutive integers to have four distinct 
;; primes factors. What is the first of these numbers?
;;

(ns euler047
  (:use [util.primes]
        [util.misc]))

(defn has-four-distinct-prime-factors? [n]
  (= 4 (count (distinct (prime-factors-of n)))))

(defn solve []
  (->> (partition 4 1 integers)
       (filter #(every? has-four-distinct-prime-factors? %)) 
       first)) 

(time (solve))
