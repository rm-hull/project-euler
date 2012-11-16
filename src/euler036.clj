;; EULER #036
;; ==========
;; The decimal number, 585 = 10010010012 (binary), is palindromic in 
;; both bases.
;;
;; Find the sum of all numbers, less than one million, which are 
;; palindromic in base 10 and base 2.
;;
;; (Please note that the palindromic number, in either base, may not 
;; include leading zeros.)
;;

(ns euler036
  (:use [util.palindromes]))

(set! *warn-on-reflection* true)

(defn to-binary [^long n]
  (if (zero? n)
    (list 0)
    (loop [n n
           bits nil]
      (if (zero? n)
        bits
        (recur (quot n 2) (cons (rem n 2) bits))))))

(defn palindromic-in-both-bases? [^long n]
  (and
    (is-palindrome? n)
    (is-palindrome? (to-binary n))))

; Only have to test odd numbers, since an even number [in binary] 
; will always end in zero, and since can never start with zero, by
; default cannot be palindromic

(defn solve []  
  (reduce + (filter palindromic-in-both-bases? (range 1 1000000 2))))

(time (solve))
