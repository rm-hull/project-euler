;; EULER #043
;; ==========
;; The number, 1406357289, is a 0 to 9 pandigital number because it is made
;; up of each of the digits 0 to 9 in some order, but it also has a rather
;; interesting sub-string divisibility property.
;;
;; Let d[1] be the 1st digit, d[2] be the 2nd digit, and so on. In this way, we
;; note the following:
;;
;;     d[2]d[3]d[4]=406 is divisible by 2
;;     d[3]d[4]d[5]=063 is divisible by 3
;;     d[4]d[5]d[6]=635 is divisible by 5
;;     d[5]d[6]d[7]=357 is divisible by 7
;;     d[6]d[7]d[8]=572 is divisible by 11
;;     d[7]d[8]d[9]=728 is divisible by 13
;;     d[8]d[9]d[10]=289 is divisible by 17
;;
;; Find the sum of all 0 to 9 pandigital numbers with this property.
;;
 
(ns euler043
  (:use [util.combinatorics]
        [util.primes]
        [util.misc]))
 
(def pandigital-seq
  (permutations (range 0 10)))
 
(defn slices [xs]
  (let [get-slice (fn [n] (to-number (take 3 (drop n xs))))]
    (map get-slice (range 1 8))))
 
(defn is-substring-divisible? [xs]
  (every? zero? (map mod (slices xs) primes)))
 
(defn calc [coll]
  (->> coll
       (filter is-substring-divisible?)
       (map to-number)
       (reduce +)))

(defn solve []
  (->> pandigital-seq
       (partition-all 20000)
       (pmap calc)
       (reduce +)))

(time (solve))
