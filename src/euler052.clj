;; EULER #052
;; ==========
;; It can be seen that the number, 125874, and its double, 251748, contain
;; exactly the same digits, but in a different order.
;;
;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
;; contain the same digits.
;; 

(ns euler052
  (:use [clojure.set]
        [util.combinatorics]
        [util.misc]))

(defn multiples-of [n]
  (iterate #(+ n %) n))

(defn check-perms [coll]
  (let [num-multiples 6
        perms (set (map to-number (permutations coll)))
        count-digits (fn [n] 
                       (count 
                         (intersection 
                           perms 
                           (set (take num-multiples (multiples-of n))))))]
    (filter #(= (count-digits %) num-multiples) perms)))

(defn solve []
  (first (flatten (pmap check-perms (combinations (range 10) 6)))))

(time (solve))
