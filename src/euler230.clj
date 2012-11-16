;; EULER #230
;; ==========
;; For any two strings of digits, A and B, we define F[A,B] to be the 
;; sequence (A,B,AB,BAB,ABBAB,...) in which each term is the concatenation
;; of the previous two.
;;
;; Further, we define D[A,B](n) to be the nth digit in the first term of 
;; F[A,B] that contains at least n digits.
;;
;; Example:
;;
;; Let A=1415926535, B=8979323846. We wish to find D[A,B](35), say.
;;
;; The first few terms of F[A,B] are:
;; 
;;  1415926535
;;  8979323846
;;  14159265358979323846
;;  897932384614159265358979323846
;;  14159265358979323846897932384614159265358979323846
;;                                    ^
;; Then D[A,B](35) is the 35th digit in the fifth term, which is 9.
;;
;; Now we use for A the first 100 digits of Ï\u20ac behind the decimal point:
;;
;;  14159265358979323846264338327950288419716939937510 
;;  58209749445923078164062862089986280348253421170679
;;
;; and for B the next hundred digits:
;;
;;  82148086513282306647093844609550582231725359408128 
;;  48111745028410270193852110555964462294895493038196 .
;;
;; Find sum(n) = 0,1,...,17   10^n x D[A,B]((127+19n) x 7^n) .
;;

(ns euler230
  (:use [util.fibonacci]
        [util.misc]))

(defn position [n] 
  (*
    (+ 127 (* 19 n))
    (iexpt 7 n)))

(defn lindenmayer-element [k] {:pre [(>= k 1)]}
  "Lindenmayer-type system sequence generator, A->B, B->AB: not valid for k < 1"
  (if (= k 1) 
    :b
    (let [q (quot k golden-ratio)
          m (* q golden-ratio)]
      ; Check to see if a multiple of the golden ratio, \u03c6 lies within the range (k-1, k)
      (if (and (> m (dec k)) (<= m k)) :b :a))))

(defn solve []
  (let [lut { :a "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
              :b "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196" } ]
    (reduce + 
      (for [n (range 0 18)
            :let [k     (dec (position n)) ; note: off-by-one counter
                  elem  (lindenmayer-element (quot k 100))
                  digit (nth (elem lut) (rem k 100))]]
        (* 
          (iexpt 10 n) 
          (char-to-int digit))))))

(time (solve))

