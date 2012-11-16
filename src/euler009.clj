;; EULER #009
;; ==========
;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;;
;;    a^2 + b^2 = c^2
;;
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.
;;
 
(ns euler009)

(defn is-rhs-triangle? [^long a ^long b ^long p]
  (let [c (- p a b)]
    (= (+ (* a a) (* b b)) (* c c))))
 
(defn solve []
  (reduce *
          (first
            (for [a (range 1 1001)
                  b (range a (- 1001 a))
                  :when (is-rhs-triangle? a b 1000)]
              (vector a b (- 1000 a b))))))
 
(time (solve))

