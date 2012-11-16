;; EULER #035
;; ==========
;; The number, 197, is called a circular prime because all rotations of the
;; digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 
;; 37, 71, 73, 79, and 97.
;;
;; How many circular primes are there below one million?
;;

(ns euler035
  (:use [util.primes]))

(defn rotations [n]
  (loop [s (seq (str n))
         len (count s)
         res nil]
    (if (zero? len)
      (map #(read-string (apply str %)) res)
      (recur (concat (rest s) [(first s)]) (dec len) (cons s res)))))

(defn is-circular-prime? [n]
  (every? is-prime? (rotations n)))

(defn solve [n batch-size]
  (letfn [(calc [coll] (count (filter is-circular-prime? coll)))]
    (reduce + (pmap calc (partition-all batch-size 
                                        (take-while #(< % n) primes))))))

(time (solve 1000000 100))
