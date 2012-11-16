;; EULER #110
;; ==========
;; In the following equation x, y, and n are positive integers.
;;
;;    1     1     1
;;   --- + --- = --- 
;;    x     y     n
;;
;; It can be verified that when n = 1260 there are 113 distinct solutions 
;; and this is the least value of n for which the total number of distinct 
;; solutions exceeds one hundred.
;; 
;; What is the least value of n for which the number of distinct solutions 
;; exceeds four million?
;; 
;; NOTE: This problem is a much more difficult version of problem 108 and as
;; it is well beyond the limitations of a brute force approach it requires a
;; clever implementation.
;; 

(ns euler110
  (:use [util.misc]
        [util.primes]))

(set! *warn-on-reflection* true)

(defn number-of-positive-solutions [n]
  (let [sqr (* n n)]
    (quot 
      (inc 
        (count-divisors sqr)) 
      2)))

(defn find-largest-dividable-num-after [n]
  (let [divisors (list 2 3 4 5 6 7 8 9 10)
        has-divisors? (fn [n] (every? #(zero? (mod n %)) divisors))]
    (->> (iterate inc n)
         (filter has-divisors?)
         first)))

(defn solve [n]
  (->> (repeat 4324320)
       (reductions +)
       (map bigint)
       (filter #(> (number-of-positive-solutions %) n))
       first))

;(time (solve 4000000))

(take 20 (map smallest-dividee integers))

(def highly-composite-numbers
  (list 1 2 4 6 12 24 36 48 60 120 180 240 360 720
        840 1260 1680 2520 5040 7560 10080 15120 20160
        25200 27720 45360 50400 55440 83160 110880
        166320 221760 277200 332640 498960 554400
        665280 720720 1081080 1441440 2162160))

(map count-divisors highly-composite-numbers)
