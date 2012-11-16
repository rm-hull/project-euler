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
  (:use [util.palindromes]
        [util.misc]))

(defn palindromic-in-both-bases? [^long n]
  (and
    (is-palindrome? n)
    (is-palindrome? (digits n 2))))

; Only have to test odd numbers, since an even number [in binary] 
; will always end in zero, and since can never start with zero, by
; default cannot be palindromic

(defn solve []  
  (->> (range 1 1000000 2)
       (filter palindromic-in-both-bases?)
       (reduce +)))

(time (solve))
