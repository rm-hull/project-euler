;; EULER #188
;; ==========
;; The hyperexponentiation or tetration of a number a by a positive integer
;; b, denoted by a↑↑b, is recursively defined by:
;;
;;    a↑↑1 = a,
;;    a↑↑(k+1) = a^(a↑↑k).
;;
;; Thus we have e.g. 3↑↑2 = 3^3 = 27, hence 3↑↑3 = 3^27 = 7625597484987 and
;; 3↑↑4 is roughly 10^(3.6383346400240996*10^12).
;;
;; Find the last 8 digits of 1777↑↑1855.
;;

(ns euler188
  (:use [util.misc]))

(defn modular-power-generator 
  "Returns a power function which limits by the supplied modulus. A 
   'modular exponentiation' calculates the remainder when a positive
   integer b (the base) raised to the e-th power (the exponent), b^e, is
   divided by a positive integer m, called the modulus. In symbols, this
   is, given base b, exponent e, and modulus m, the modular exponentiation
   c is: c ≡ b^e (mod m)"
  [^long modulus]
  (fn [^long x ^long exp]
    (loop [n   exp
           res 1]
      (if (zero? n)
        res
        (recur 
          (dec n)
          (let [x1 (* x res)]
             (long (if (> x1 modulus) (rem x1 modulus) x1))))))))

(defn solve [n digits]
  (let [modulus      (iexpt 10 digits)
        tetration-fn (partial (modular-power-generator modulus) n)]
    (loop [prev      nil
           solutions (iterate tetration-fn n)]
      (if (= prev (first solutions))
        prev
        (recur
          (first solutions) 
          (next solutions))))))

(time (solve 1777 8))
