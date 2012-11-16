;; EULER #053
;; ==========
;; There are exactly ten ways of selecting three from five, 12345:
;;
;;     123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;;
;; In combinatorics, we use the notation, 5C3 = 10.
;;
;; In general,
;;
;;               n!
;;     nCr = ----------  , where r <= n, n!=n x (n-1) x ... x 3 x 2 x 1
;;            r!(n-r)!	                   and 0! = 1
;;
;; It is not until n = 23, that a value exceeds one-million: 
;; 23C10 = 1144066.
;;
;; How many, not necessarily distinct, values of  nCr, for 1 <= n <= 100,
;; are greater than one-million?
;;

(ns euler053)

(def factorial
 (memoize
  (fn [n]
    (apply * (range 1N (inc n))))))
 
(defn ncr [n r]
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defn solve []
  (count 
    (for [n (range 1 101)
          r (range 1 (inc n))
          :let [ncr (ncr n r)]
          :when (> ncr 1000000)]
      ncr)))

(time (solve))
