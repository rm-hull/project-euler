;; EULER #108
;; ==========
;; In the following equation x, y, and n are positive integers.
;;
;;    1     1     1
;;   --- + --- = --- 
;;    x     y     n
;;
;; For n = 4 there are exactly three distinct solutions:

;;    1     1     1
;;   --- + --- = --- 
;;    5     20    4
;;
;;    1     1     1
;;   --- + --- = --- 
;;    6     12    4
;;
;;    1     1     1
;;   --- + --- = --- 
;;    8     8     4
;;
;; What is the least value of n for which the number of distinct solutions
;; exceeds one-thousand?
;;
;; NOTE: This problem is an easier version of problem 110; it is strongly
;; advised that you solve this one first.
;;

(ns euler108
  (:use [util.misc]
        [util.primes]))

(set! *warn-on-reflection* true)

(defn number-of-positive-solutions [^long n]
  (let [sqr (* n n)]
    (quot 
      (inc 
        (count-divisors sqr)) 
      2)))

(defn find-largest-dividable-num-after [n]
  (let [divisors (list 2 3 4 5 6 7 9 10)
        has-divisors? (fn [n] (every? #(zero? (mod n %)) divisors))]
    (->> (iterate inc n)
         (filter has-divisors?)
         first)))

(defn solve [n]
  (->> (find-largest-dividable-num-after n)
       repeat
       (reductions +)
       (filter #(> (number-of-positive-solutions %) n))
       first))

(time (solve 1000))
