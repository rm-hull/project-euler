;; EULER #119
;; ==========
;; The number 512 is interesting because it is equal to the sum of its digits
;; raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a 
;; number with this property is 614656 = 28^4.
;;
;; We shall define a[n] to be the nth term of this sequence and insist that a
;; number must contain at least two digits to have a sum.
;; 
;; You are given that a[2] = 512 and a[10] = 614656.
;;
;; Find a[30].
;;

(defn sum-of-digits [n]
  (loop [acc 0
         n n]
    (if (zero? n) 
      acc
      (recur (+ acc (mod n 10)) (long (/ n 10))))))

(defn interesting-sums [n]
  (let [max-power 8
        powers (rest (reductions * (repeat max-power n)))]
    (filter #(= n (second %)) 
            (map #(vector % (sum-of-digits %)) powers))))

(defn solve [n]
  (let [idx (dec n)]
    (nth (sort (map first (mapcat interesting-sums (range 6 70)))) idx)))

(time (solve 30))

