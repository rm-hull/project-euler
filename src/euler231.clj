;; EULER #231
;; ==========
;; The binomial coefficient 10C3 = 120.
;;
;;      120 = 2^3 x 3 x 5 = 2 x 2 x 2 x 3 x 5, and 2 + 2 + 2 + 3 + 5 = 14.
;;
;; So the sum of the terms in the prime factorisation of 10C3 is 14. 
;;
;; Find the sum of the terms in the prime factorisation of 20000000C15000000.

(ns euler231
  (:use [util.primes]))

(defn binomial [n k]
  ;; Helper function to compute C(n,k) via forward recursion
  (letfn [(iter [n k i prev]
            (if (>= i k)
              prev
              (recur n k (inc i) (quot (* (- n i) prev) (inc i)))))]
  ;; Use symmetry property C(n,k)=C(n, n-k)
  (if (< k (- n k))
    (iter n k 0N 1N)
    (iter n (- n k) 0N 1N))))

(defn binomial [n k]
  (cond 
    (> k n) 0
    (> (* k 2) n) (binomial n (- n k))
    (zero? k) 1)
    :else (loop [i   2
                 res n]
            (if (> i k)
              res
              (recur 
                (inc i) 
                (quot (* res (inc (- n i))) i)))))


(defn binomial-prime-factors [n k]
  (letfn [(freqs [from to] (frequencies (mapcat prime-factors-of (range from (inc to)))))]
    (merge-with -
      (freqs (inc k) n)
      (freqs 2 (- n k)))))


(defn prime-power [^long p ^long n]
  (loop [m   p
         res 0]
    (if (> m n)
      res
      (recur 
        (* m p) 
        (+ res (quot n m))))))

(defn binomial-prime-factors [n k]
  (let [n-minus-k (- n k)]
    (for [p (primes-range 2 n)
          :let [t (- (prime-power p n)
                     (prime-power p k)
                     (prime-power p n-minus-k))]
          :when (pos? t)]
      [p t])))

(defn factorial-prime-factors [n]
  (for [p (primes-range 2 n)]
      [p (prime-power p n)]))

(defn solve [n k]
  (->> (binomial-prime-factors n k)
       (map (partial apply *))
       (reduce +)))

;(time (solve 10 3))

;(time (solve 20e6 15e6))

