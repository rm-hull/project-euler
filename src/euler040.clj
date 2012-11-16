;; EULER #040
;; ==========
;; An irrational decimal fraction is created by concatenating the positive
;; integers:
;;
;; 0.123456789101112131415161718192021...
;;              ^
;;
;; It can be seen that the 12th digit of the fractional part is 1.
;;
;; If d[n] represents the nth digit of the fractional part, find the value of
;; the following expression.
;;
;; d[1] x d[10] x d[100] x d[1000] x d[10000] x d[100000] x d[1000000]
;;

(ns euler040
  (:use [util.misc]))
 
(def digit-seq 
  (mapcat digits integers))
 
(defn solve [n]
  (let [indexes (into (sorted-set) (take n (reductions * 1 (repeat 10))))]
    (->> (map vector integers digit-seq)
         (filter #(indexes (first %)))
         (take n)
         (map second)
         (reduce *))))
 
(time (solve 7))
