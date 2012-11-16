;; EULER #063
;; ==========
;; The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the
;; 9-digit number, 134217728=8^9, is a ninth power.
;;
;; How many n-digit positive integers exist which are also an nth power?
;;

(ns euler063
  (:use [util.misc]))

(defn digit-powers [n]
  (take-while #(= (:pow %) (count (str (:val %))))
    (map #(array-map :n %1 :pow %2 :val %3)
         (repeat n)
         integers
         (reductions * (repeat (bigint n))))))

(defn solve []
  (let [rng  integers
        data (reduce concat (take-while not-empty (map digit-powers rng)))]
    { :count (count data) :data data }))

(time (solve))

