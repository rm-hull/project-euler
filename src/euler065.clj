;; EULER #065
;; ==========
;; The square root of 2 can be written as an infinite continued fraction.
;;
;; sqrt(2) = 1 +    1
;;              ============== 
;;              2 +   1
;;                 =========== 
;;                 2 +  1
;;                    ========
;;                    2 +  1
;;                       ======
;;                       2 + ...
;;
;; The infinite continued fraction can be written, 2 = [1;(2)], (2) 
;; indicates that 2 repeats ad infinitum. In a similar way, 
;; 23 = [4;(1,3,1,8)].
;;
;; It turns out that the sequence of partial values of continued fractions
;; for square roots provide the best rational approximations. Let us 
;; consider the convergents for sqrt(2).
;;
;; Hence the sequence of the first ten convergents for 2 are:
;;
;;    1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 
;;    1393/985, 3363/2378, ...
;;
;; What is most surprising is that the important mathematical constant,
;; e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
;;
;; The first ten terms in the sequence of convergents for e are:
;; 
;;     2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 
;;     1457/536, ...
;;
;; The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
;;
;; Find the sum of digits in the numerator of the 100th convergent 
;; of the continued fraction for e.
;;

(ns euler065
  (:use [util.misc]))

(def e-repeating-seq
  (lazy-seq (cons 2 (mapcat #(vector %1 (* 2 %2) %1) (repeat 1) integers))))

(def sqrt2-repeating-seq
  (lazy-seq (cons 1 (repeat 2))))

(defn infinite-continued-fraction [s iterations]
  (let [s (reverse (take iterations s))]
    (loop [xs (rest s)
           result (first s)]
      (if (seq xs)
        (recur (rest xs) (+ (first xs) (/ 1 result)))
        result))))

(defn infinite-continued-fraction-seq [s]
  (let [f (partial infinite-continued-fraction s)]
    (map f (iterate inc 1))))

(defn e [iterations]
  (nth 
    (infinite-continued-fraction-seq e-repeating-seq) 
    (dec iterations)))

(defn solve [iterations]
  (reduce + (map char-to-int (str (numerator (e iterations))))))

(time (solve 100))
