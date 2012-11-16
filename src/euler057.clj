;; EULER #057
;; ==========
;; It is possible to show that the square root of two can be expressed
;; as an infinite continued fraction.
;;
;;     sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;;
;; By expanding this for the first four iterations, we get:
;; 
;;     1 + 1/2 = 3/2 = 1.5
;;     1 + 1/(2 + 1/2) = 7/5 = 1.4
;;     1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;;     1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;;
;; The next three expansions are 99/70, 239/169, and 577/408, but the
;; eighth expansion, 1393/985, is the first example where the number of
;; digits in the numerator exceeds the number of digits in the denominator.
;; 
;; In the first one-thousand expansions, how many fractions contain a 
;; numerator with more digits than denominator?
;;

(ns euler057)

(defn sqrt-of-two [iterations]
  (loop [n 0
         result (/ 3 2)]
    (if (>= n iterations)
      result
      (recur 
        (inc n) 
        (+ 1 (/ 1 (+ 1 result)))))))

(def sqrt2-seq
  (pmap sqrt-of-two (iterate inc 0)))

(defn digits-diff [ratio]
  (let [n (numerator ratio)
        d (denominator ratio)
        size (fn [n] (count (str n)))]
    (- (size n) (size d))))

(defn solve [n]
  (count (filter #(pos? (digits-diff %)) (take n sqrt2-seq))))

(time (solve 1000))
