;; EULER #071
;; ==========
;; Consider the fraction, n/d, where n and d are positive integers. 
;; If n < d and HCF(n,d)=1, it is called a reduced proper fraction.
;;
;; If we list the set of reduced proper fractions for d <= 8 in ascending 
;; order of size, we get:
;;
;;     1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 
;;                   4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.
;;
;; By listing the set of reduced proper fractions for d <= 1,000,000 in 
;; ascending order of size, find the numerator of the fraction immediately
;; to the left of 3/7.
;;

(ns euler071)

(defn farey-seq [^long n]
  (letfn [(next-farey-num [^long a ^long b ^long c ^long d] 
            (let [k (quot (+ n b) d)
                  p (- (* k c) a)
                  q (- (* k d) b)]
              (if (< c n) (cons (/ c d) (lazy-seq (next-farey-num c d p q))))))]
    (cons 0 (next-farey-num 0 1 1 n))))

(defn half-down-reverse-farey-seq [^long n]
  (letfn [(prev-farey-num [^long a ^long b ^long c ^long d] 
            (let [k (quot (+ n b) d)
                  p (- (* k c) a)
                  q (- (* k d) b)]
              (lazy-seq 
                (if (pos? a) (cons (/ c d) (prev-farey-num c d p q))))))]
    (cons (/ 1 2) (prev-farey-num 1 2 (quot n 2) (inc n)))))

(defn farey-mediant [a b]
  (/ (+ (numerator a) (numerator b))
     (+ (denominator a) (denominator b))))

(defn solve [n]
  (->> (repeat (/ 3 7))
       (reductions farey-mediant (/ 2 5))
       (take-while #(<= (denominator %) n))
       (last)))

(time (solve 1000000))
