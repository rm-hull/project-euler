;; EULER #097
;; ==========
;; The first known prime found to exceed one million digits was discovered in
;; 1999, and is a Mersenne prime of the form 2^6972593-1; it contains exactly 
;; 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p-1, 
;; have been found which contain more digits.
;;
;; However, in 2004 there was found a massive non-Mersenne prime which 
;; contains 2,357,207 digits: 28433x2^7830457+1.
;;
;; Find the last ten digits of this prime number.
;;

(use 'clojure.contrib.math)

(defn power [digits]
  (loop [i   7830457
         acc 28433]
    (if (zero? i)
      acc
      (recur (dec i) (mod (* 2 acc) digits)))))

(defn solve [digits]
  (let [n (expt 10 digits)]
    (inc (power n))))

(time (solve 10))
