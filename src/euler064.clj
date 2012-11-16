;; EULER #064
;; ==========
;; All square roots are periodic when written as continued fractions and can
;; be written in the form:
;; 
;;    sqrt(N) = a0 +    1
;;                  ============== 
;;                  a1 +   1
;;                      =========== 
;;                      a2 +  1
;;                          ========
;;                          a3 +  1
;;                              ======
;;                              a4 + ...
;;
;; For example, let us consider 23. If we continue we would get the
;; following expansion:
;;
;;    sqrt(23) = 4 +    1
;;                   ============== 
;;                   1 +   1
;;                      =========== 
;;                      3 +  1
;;                         ========
;;                         1 +  1
;;                            ======
;;                            8 + ...
;;
;; It can be seen that the sequence is repeating. For conciseness, we use
;; the notation 23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) 
;; repeats indefinitely.
;; 
;; The first ten continued fraction representations of (irrational) square 
;; roots are:
;;
;;    sqrt(2)=[1;(2)], period=1
;;    sqrt(3=[1;(1,2)], period=2
;;    sqrt(5)=[2;(4)], period=1
;;    sqrt(6)=[2;(2,4)], period=2
;;    sqrt(7)=[2;(1,1,1,4)], period=4
;;    sqrt(8)=[2;(1,4)], period=2
;;    sqrt(10)=[3;(6)], period=1
;;    sqrt(11)=[3;(3,6)], period=2
;;    sqrt(12)= [3;(2,6)], period=2
;;    sqrt(13)=[3;(1,1,1,1,6)], period=5
;;
;; Exactly four continued fractions, for N <= 13, have an odd period.
;;
;; How many continued fractions for N <= 10000 have an odd period?
;;

(ns euler064
  (:use [util.misc]))

(def a)
(def d)
(def m)
 
(def a
  (memoize
    (fn [^long s ^long n]
      (if (zero? n)
        (int (Math/sqrt s))
        (int (/ (+ (a s 0) (m s n)) (d s n)))))))
 
(def d
  (memoize
    (fn [^long s ^long n]
      (if (zero? n)
        1
        (/ (- s (iexpt (m s n) 2)) (d s (dec n)))))))
 
(def m
  (memoize
    (fn [^long s ^long n]
      (let [n1 (dec n)]
        (if (zero? n)
          0
          (- (* (d s n1) (a s n1)) (m s n1)))))))
 
(defn sqrt-fractional-expansion [n]
  (let [sqrt (a n 0)]
    (if (= n (* sqrt sqrt))
      (list sqrt)
      (map (partial a n) (iterate inc 0)))))
 
(defn period-of [n]
  (let [expansion (sqrt-fractional-expansion n)
        repeating-num (* 2 (first expansion))]
    (if (empty? (rest expansion))
      0
      (->> expansion
           (take-while (partial not= repeating-num))
           count))))

(defn solve [n]
  (->> (range (inc n))
       (map period-of)
       (filter odd?)
       count))

(time (solve 10000))

