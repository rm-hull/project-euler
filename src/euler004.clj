;; EULER #004
;; ==========
;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is
;; 9009 = 91 x 99.
;;
;; Find the largest palindrome made from the product of two
;; 3-digit numbers.
;;

(ns euler004
  (:use [util.palindromes]))

(defn solve []
  (reduce max
    (for [a (range 100 1000)
          b (range a 1000)
          :let [sum (* a b)]
          :when (is-palindrome? sum)]
      sum)))

(time (solve))
