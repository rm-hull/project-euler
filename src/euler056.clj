;; EULER #056
;; ==========
;; A googol (10^100) is a massive number: one followed by one-hundred 
;; zeros; 100^100 is almost unimaginably large: one followed by two-hundred
;; zeros. Despite their size, the sum of the digits in each number is only
;; 1.
;;
;; Considering natural numbers of the form, a^b, where a, b < 100, what is
;; the maximum digital sum?
;;

(ns euler056
  (:use [util.misc]))

(defn sum-of-digits [n]
    (reduce + (map char-to-int (str n))))

(defn solve [n]
  (reduce max 
    (for [a (range n)
          b (range n)]
      (sum-of-digits (iexpt (bigint a) b)))))

(time (solve 100N))
