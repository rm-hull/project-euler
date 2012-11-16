;; EULER #134
;; ==========
;; Consider the consecutive primes p1 = 19 and p2 = 23. It can be verified 
;; that 1219 is the smallest number such that the last digits are formed by
;; p1 whilst also being divisible by p2.
;;
;; In fact, with the exception of p1 = 3 and p2 = 5, for every pair of 
;; consecutive primes, p2 > p1, there exist values of n for which the last 
;; digits are formed by p1 and n is divisible by p2. Let S be the smallest 
;; of these values of n.
;;
;; Find sum(S) for every pair of consecutive primes with 5 <= p1 <= 1000000.
;;

(ns euler134
  (:use [util.primes]))

(defn pairs-of-primes [limit]
  (->> primes
      (drop-while #(< % 5))
      (partition 2 1)
      (take-while #(<= (first %) limit))))

(defn last-digits [^long a]
  (let [increment (reduce * (repeat (count (str a)) 10))
        start     (+ a increment)]
    (iterate (partial + increment) start)))

(defn s [[^long a ^long b]]
  (->> (last-digits a)
       (filter #(zero? (mod % b)))
       first))

(defn s [[^long a ^long b]]
  (let [bs  (rest (reductions + (repeat b)))
        m   (reduce * (repeat (count (str a)) 10))
        flt (fn [x] (= (mod x m) a))]
    (->> bs
         (filter flt)
         first)))

(defn calc [coll]
  (->> (map s coll)
       (reduce +)))

(defn solve [n]
  (->> (pairs-of-primes n)
       (partition-all 1000)
       (pmap calc)
       (reduce +)))

;(drop-while #(< (first %) 999000)
;  (pairs-of-primes 1000000))

;(time (solve 1000000))

  (->> (pairs-of-primes 1000)
       (map s))
  (->> (pairs-of-primes 1000)
       (map s2))

  (->> (pairs-of-primes 1000)
       (map #(vector % (/ (s %) (second %)))))

  (->> (pairs-of-primes 1000)
       (map #(vector % (s %) (reduce  * %))))

(take 20 (drop-while #(<= % (* 977 983)) (last-digits 977)))
(first (filter #(= (mod % 1000) 977) (reductions + (repeat 983))))


(s [977 983])

(take 53 (reductions + (repeat 23)))

(take 50 (last-digits 19))

(count (pairs-of-primes 10000))

;(time (solve 1000000))
;"Elapsed time: 3229949.908 msecs"
;18613426663617118


