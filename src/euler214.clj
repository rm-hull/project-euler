;; EULER #214
;; ==========
;; Let φ be Euler's totient function, i.e. for a natural number n, φ(n) is
;; the number of k, 1 <= k <= n, for which gcd(k,n) = 1.
;;
;; By iterating φ, each positive integer generates a decreasing chain of 
;; numbers ending in 1. E.g. if we start with 5 the sequence 5,4,2,1 is 
;; generated. Here is a listing of all chains with length 4:
;;
;;    5,4,2,1
;;    7,6,2,1
;;    8,4,2,1
;;    9,6,2,1
;;    10,4,2,1
;;    12,4,2,1
;;    14,6,2,1
;;    18,6,2,1
;;
;; Only two of these chains start with a prime, their sum is 12.
;;
;; What is the sum of all primes less than 40000000 which generate a chain
;; of length 25?
;;

(ns euler214
  (:use [util.primes]))

(defn totient-chain 
  "Only good for primes, as it makes use of the fact that φ(p) = p-1,
   where p is prime"
  [^long n]
  (loop [i (dec n)
         res [i n]]
    (if (= i 1)
      res
      (let [totient (phi i)]
        (recur totient (cons totient res))))))

(def totient-chain
  (memoize
    (fn [^long n]
      (if (= n 1)
        [1]
        (cons n (totient-chain (phi n)))))))

(defn solve [limit len]
  (->> (primes-range 2 limit)
       (filter #(= len (count (totient-chain %))))
       (reduce +)))

(time (solve 18 4))

(time (solve 10000 10))

(time (solve 4e7 25))


