;; EULER #125
;; ==========
;; The palindromic number 595 is interesting because it can be written as
;; the sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
;;
;; There are exactly eleven palindromes below one-thousand that can be 
;; written as consecutive square sums, and the sum of these palindromes 
;; is 4164. Note that 1 = 0^2 + 1^2 has not been included as this problem
;; is concerned with the squares of positive integers.
;;
;; Find the sum of all the numbers less than 10^8 that are both palindromic
;; and can be written as the sum of consecutive squares.
;;

(ns euler125
  (:use [util.palindromes]))

(set! *warn-on-reflection* true)

(defn sum-of-squares [^long n]
  (quot 
    (*
      n
      (inc n)
      (inc (+ n n)))
    6))

(defn sum-of-consecutive-squares [n m]
  (- (sum-of-squares m)
     (sum-of-squares (dec n))))

(defn solve [n]
  (let [limit (Math/sqrt n)]
    (reduce + 
      (distinct
        (for [a (range 1 limit)
              b (range (inc a) limit)
              :let [sum (sum-of-consecutive-squares a b)]
              :when (and
                      (< sum n)
                      (is-palindrome? sum))]
          sum)))))

(time (solve 100000000))
