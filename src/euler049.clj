;; EULER #049
;; ==========
;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
;; increases by 3330, is unusual in two ways: (i) each of the three terms
;; are prime, and, (ii) each of the 4-digit numbers are permutations of
;; one another.
;;
;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
;; primes, exhibiting this property, but there is one other 4-digit
;; increasing sequence.
;;
;; What 12-digit number do you form by concatenating the three terms in
;; this sequence?
;;
 
(ns euler049
  (:use [util.combinatorics]
        [util.primes]))

(def four-digit-primes
  (->> primes 
       (drop-while #(< % 1000))
       (take-while #(< % 10000)))) 
 
(defn normalize [n]
  (sort (str n)))
 
(defn deltas [xs]
  (let [ordered (sort xs)]
    (map #(- %2 %1) ordered (next ordered))))
 
(defn same-delta [xs]
    (= 1 (count (distinct (deltas xs)))))
 
(defn solve []
  (map #(read-string (apply str %))
       (mapcat #(filter same-delta (combinations % 3))
               (vals (group-by normalize four-digit-primes)))))
 
(time (solve))

