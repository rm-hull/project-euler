;; EULER #041
;; ==========
;; We shall say that an n-digit number is pandigital if it makes use 
;; of all the digits 1 to n exactly once. For example, 2143 is a 4-digit
;; pandigital and is also prime.
;; 
;; What is the largest n-digit pandigital prime that exists?
;;

(ns euler041
  (:use [util.combinatorics]
        [util.misc]
        [util.primes]))

(defn pandigital-seq [n]
  (let [seeds (range n 0 -1)]
    (map to-number (permutations seeds))))
 
(defn solve [n]
  (->> (range n 1 -1)
       (mapcat pandigital-seq) 
       (filter odd?)
       (filter is-prime?)
       first))
  
(time (solve 9))
