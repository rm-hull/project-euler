;; EULER #066
;; ==========
;; Consider quadratic Diophantine equations of the form:
;;
;;    x^2 - Dy^2 = 1
;;
;; For example, when D=13, the minimal solution in x is 649^2 - 13x180^2 = 1.
;;
;; It can be assumed that there are no solutions in positive integers when D 
;; is square.
;; 
;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the 
;; following:
;; 
;;    3^2 - 2x2^2 = 1
;;    2^2 - 3x1^2 = 1
;;    9^2 - 5x4^2 = 1
;;    5^2 - 6x2^2 = 1
;;    8^2 - 7x3^2 = 1
;;
;; Hence, by considering minimal solutions in x for D  7, the largest x is 
;; obtained when D=5.
;; 
;; Find the value of D <= 1000 in minimal solutions of x for which the largest
;; value of x is obtained.

(ns euler066
  (:use [util.misc]))

(def a)
(def d)
(def m)
 
(def a
  (memoize
    (fn [s n]
      (if (zero? n)
        (int (Math/sqrt s))
        (int (/ (+ (a s 0) (m s n)) (d s n)))))))
 
(def d
  (memoize
    (fn [s n]
      (if (zero? n)
        1
        (/ (- s (iexpt (m s n) 2)) (d s (dec n)))))))
 
(def m
  (memoize
    (fn [s n]
      (let [n1 (dec n)]
        (if (zero? n)
          0
          (- (* (d s n1) (a s n1)) (m s n1)))))))
 
(defn sqrt-fractional-expansion [n]
  (map (partial a n) (iterate inc 0)))
 
(defn infinite-continued-fraction [s iterations]
  (let [s (reverse (take iterations s))]
    (loop [xs (rest s)
           result (first s)]
      (if (seq xs)
        (recur (rest xs) (+ (first xs) (/ 1 result)))
        result))))

(defn infinite-continued-fraction-seq [s]
  (map (partial infinite-continued-fraction s) integers))

(defn is-fundamental-solution? [d surd]
  (let [x (if (ratio? surd) (numerator surd) surd)
        y (if (ratio? surd) (denominator surd) 1)]
    (= 1 (- (* x x) (* d y y )))))

(defn get-minimal-solution [n]
  (let [surd (->> (sqrt-fractional-expansion n)
                  (infinite-continued-fraction-seq)
                  (filter (partial is-fundamental-solution? n))
                  (first))] 
    (if (ratio? surd)
      [(numerator surd) (denominator surd)]
      [surd 1])))

(defn is-perfect-square? [n]
  (let [sqrt (Math/sqrt n)]
    (== (int sqrt) sqrt)))

(defn solve [n]
  (->> (iterate inc 1)
       (remove is-perfect-square?)
       (take-while #(<= % n))
       (map #(cons % (get-minimal-solution %)))
       (reduce (fn [a b] (if (> (second a) (second b)) a b)))))

(time (solve 1000))
