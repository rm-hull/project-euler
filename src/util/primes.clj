(ns util.primes)

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b] (/ (* a b) (gcd a b)))

(defn coprime? [a b] (= 1 (gcd a b)))

(def is-prime?
  (memoize
    (fn [^long n] 
        (cond
          (<= n 1) false
          (=  n 2) true
          :else    (let [r (range 2 (inc (Math/sqrt n)))]
                     (every? false? (map #(zero? (rem n %)) r)))))))

(def primes
  (concat 
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
      (fn primes-from [n [f & r]]
        (if (some #(zero? (rem n %))
                  (take-while #(<= (* % %) n) primes))
          (recur (+ n f) r)
          (lazy-seq (cons n (primes-from (+ n f) r)))))
      wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                    6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                    2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn prime-factors-of [^long n]
  (let [q (long (Math/sqrt n))]
    (loop [n n
           p primes
           res nil]
      (let [d (long (first p))]
        (cond
          (or (> d q) (= n d)) (cons n res)
          (zero? (rem n d))    (recur (quot n d) p (cons d res))
          :else                (recur n (next p) res))))))

(def composites 
  (remove is-prime? (iterate inc 2)))

(defn primes-after [n]
  (let [next-prime (.nextProbablePrime (BigInteger/valueOf n))]
    (cons next-prime (lazy-seq (primes-after next-prime)))))

(defn primes-range [x y]
  (->> primes
       (drop-while #(< % x))
       (take-while #(<= % y))))

(defn quadratfrei? 
  "A number is said to be squarefree (or sometimes quadratfrei; Shanks 1993)
   if its prime decomposition contains no repeated factors. All primes are 
   therefore trivially squarefree. The number 1 is by convention taken to be
   squarefree. The squarefree numbers are 1, 2, 3, 5, 6, 7, 10, 11, 13, 14,
   15, ... (Sloane's A005117). The squareful numbers (i.e., those that 
   contain at least one square) are 4, 8, 9, 12, 16, 18, 20, 24, 25, ... 
   (Sloane's A013929)."
  [n]
  (->> (prime-factors-of n)
       frequencies
       (every? #(= (second %) 1))))
  ;(let [sqrt (inc (long (Math/sqrt n)))]
  ;  (->> (iterate inc 1)
  ;       (filter odd?)
  ;       (reductions +)
  ;       next
  ;       (take-while #(<= % n))
  ;       (every? #(pos? (rem n %))))))

(defn phi 
  "Euler's totient or phi function, φ(n) is an arithmetic function that
   counts the number of positive integers less than or equal to n that 
   are relatively prime to n. That is, if n is a positive integer, then 
   φ(n) is the number of integers k in the range 1 ≤ k ≤ n for which 
   gcd(n, k) = 1"
  [^long n]
  (->> (prime-factors-of n)
       distinct
       (map #(- 1 (/ 1 %))) 
       (reduce * n)
       long))
