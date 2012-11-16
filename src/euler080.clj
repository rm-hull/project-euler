;; EULER #080
;; ==========
;; It is well known that if the square root of a natural number is not an
;; integer, then it is irrational. The decimal expansion of such square 
;; roots is infinite without any repeating pattern at all.
;;
;; The square root of two is 1.41421356237309504880..., and the digital 
;; sum of the first one hundred decimal digits is 475.
;;
;; For the first one hundred natural numbers, find the total of the digital
;; sums of the first one hundred decimal digits for all the irrational 
;; square roots.
;;

(ns euler080
  (:use [util.misc]))

(defn number-pairs [n]
  (let [join (fn [[a b]] (+ (* 10 a) b))
        d (digits n)
        normalized (if (odd? (count d)) (cons 0 d) d)]
    (concat 
      (map join (partition 2 normalized))
      (repeat 0))))

(defn calc-y [x p]
  (* x (+ (* 20 p) x)))

(defn biggest-x [c p]
  (->> (iterate inc 0)
       (take-while #(<= (calc-y % p) c))
       last))

(defn- sqrt0 [xs r p]
  (let [c (+ (* r 100) (first xs))
        x (biggest-x c p)
        y (calc-y x p)
        r (- c y)
        p (+ (* 10 p) x)]
    (lazy-seq 
      (if (and (zero? c) (zero? r)) 
        nil 
        (cons x (sqrt0 (next xs) r p))))))

(defn sqrt-digits-seq [n]
  (let [xs (number-pairs n)]
    (sqrt0 xs 0N 0N)))

(defn irrational-roots [limit]
  (let [perfect-squares (set (take-while #(<= % limit) squares))]
    (->> (range 1 (inc limit))
         (remove perfect-squares))))

(defn solve [n digits]
  (->> (irrational-roots n)
       (mapcat #(take digits (sqrt-digits-seq %)))
       (reduce +)))

(time (solve 100 100))
