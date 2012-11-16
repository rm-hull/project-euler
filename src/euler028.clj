;; EULER #028
;; ==========
;; Starting with the number 1 and moving to the right in a clockwise
;; direction a 5 by 5 spiral is formed as follows:
;;
;;   21 22 23 24 25
;;   20  7  8  9 10
;;   19  6  1  2 11
;;   18  5  4  3 12
;;   17 16 15 14 13
;;
;; It can be verified that the sum of the numbers on the diagonals is 101.
;;
;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
;; formed in the same way?)
;;

(ns euler028
  (:use [util.misc]))

(def spiral-seq
  (mapcat #(repeat 4 %) (map #(* % 2) integers)))

(defn accumulate [coll n]
  (concat coll [(+ (last coll) n)]))

(defn solve [n]
  (reduce + (reduce accumulate (list 1) (take-while #(<= % n) spiral-seq))))

(time (solve 1001))
