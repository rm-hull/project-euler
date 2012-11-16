;; EULER #225
;; ==========
;; The sequence 1, 1, 1, 3, 5, 9, 17, 31, 57, 105, 193, 355, 653, 1201 ...
;; is defined by T[1] = T[2] = T[3] = 1 and T[n] = T[n-1] + T[n-2] + T[n-3].
;;
;; It can be shown that 27 does not divide any terms of this sequence.
;; In fact, 27 is the first odd number with this property.
;;
;; Find the 124th odd number that does not divide any terms of the above
;; sequence.
;;

(ns euler225
  (:use [util.misc]
        [util.fibonacci]))

(defn non-divisor [^long n]
  (loop [a 1
         b 1
         c 1
         i (* n n)]
    (let [t (rem (+ a b c) n)]
      (cond 
        (zero? i)   false
        (zero? t)   false
        (= b c t 1) true
        :else       (recur b c t (dec i))))))

(def A046735		 
  "Nontrivial (i.e. having no nontrivial factors with this property) integers
   which do not divide any terms of A000213 [tribonacci numbers]."
  (->> integers
       (filter odd?)
       (filter non-divisor)))

(defn solve [n]
  (nth A046735 (dec n)))

(time (solve 124))
