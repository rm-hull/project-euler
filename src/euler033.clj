;; EULER #033
;; ==========
;; The fraction 49/98 is a curious fraction, as an inexperienced 
;; mathematician in attempting to simplify it may incorrectly believe 
;; that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
;;
;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
;;
;; There are exactly four non-trivial examples of this type of fraction,
;; less than one in value, and containing two digits in the numerator and
;; denominator.
;;
;; If the product of these four fractions is given in its lowest common 
;; terms, find the value of the denominator.
;;

(ns euler033
  (:use [util.misc]))

(defn curious-fraction? [n d]
  (let [ns (digits n)
        ds (digits d)]
    (and 
      (pos? (second ds))
      (= (second ns) (first ds))
      (= (/ n d) (/ (first ns) (second ds))))))

(defn solve [n]
  (reduce * 
          (for [a (range 10 n)
                b (range (inc a) n)
                :when (curious-fraction? a b)]
            (/ a b))))

(time (solve 100))
