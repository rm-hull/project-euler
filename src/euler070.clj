;; EULER #070
;; ==========
;; Euler's Totient function, φ(n) [sometimes called the phi function], is
;; used to determine the number of positive numbers less than or equal to
;; n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8,
;; are all less than nine and relatively prime to nine, φ(9)=6.
;;
;; The number 1 is considered to be relatively prime to every positive number,
;; so φ(1)=1.
;;
;; Interestingly, φ(87109)=79180, and it can be seen that 87109 is a
;; permutation of 79180.
;;
;; Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n
;; and the ratio n/φ(n) produces a minimum.
;;

(ns euler070
  (:use [util.primes]))

(defn search-space [limit]
  (let [root (Math/sqrt limit)
        lower-bound (* 0.3 root)
        upper-bound (* 1.2 root)
        primes-range (fn [a b] 
                      (->> primes 
                           (drop-while #(< % lower-bound))
                           (take-while #(< % upper-bound))))]
  (for [a (primes-range lower-bound upper-bound)
        b (primes-range a upper-bound)
        :when (<= (* a b) limit)]
    [a b])))

(defn phi [[a b]]
  "a and b must be prime factors"
  (* (dec a) (dec b)))

(defn are-permutations? [a b]
  (let [sort-digits (fn [n] (sort (str n)))]
    (= (sort-digits a) (sort-digits b))))

(defn solve [n]
  (first 
    (sort-by second
      (for [prime-factors (search-space n)
            :let [x   (apply * prime-factors)
                  phi (phi prime-factors)]
            :when (are-permutations? x phi)]
        [x (double (/ x phi))]))))

(time (solve 10000000))
