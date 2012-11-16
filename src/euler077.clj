;; EULER #077
;; ==========
;; It is possible to write ten as the sum of primes in exactly five 
;; different ways:
;;
;;    7 + 3
;;    5 + 5
;;    5 + 3 + 2
;;    3 + 3 + 2 + 2
;;    2 + 2 + 2 + 2 + 2
;;
;; What is the first value which can be written as the sum of primes
;; in over five thousand different ways?
;;

(ns euler077
  (:use [util.primes]
        [util.misc]))

(def integer-sum
  (memoize
    (fn [c v]
      (let [f (first c)]
        (cond 
          (nil? f) 0
          (and (= f (last c)) (zero? (mod v f))) 1
          :else (reduce + 
                        (for [n (range 0 (inc (quot v f)))]
                          (integer-sum (rest c) (- v (* n f))))))))))

(defn get-ways [n]
  (let [p (reverse (take-while #(< % n) primes))]
    (integer-sum p n)))

(defn solve [n]
  (first 
    (filter #(> (get-ways %) n) integers)))

(time (solve 5000))
