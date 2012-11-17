;; EULER #160
;; ==========
;; For any N, let f(N) be the last five digits before the trailing zeroes
;; in N!. For example,
;;
;;    9! = 362880 so f(9)=36288
;;    10! = 3628800 so f(10)=36288
;;    20! = 2432902008176640000 so f(20)=17664
;;
;; Find f(1,000,000,000,000)
;;

(ns euler160
  (:use [util.misc]))

(defn drop-trailing-zeros [^long n]
  (loop [n n]
    (if (pos? (rem n 10))
      n
      (recur (quot n 10)))))

(defn last-n-digits [^long n]
  (let [m (iexpt 10 n)]
    (fn [^long a] (rem a m))))

(def truncate 
  (comp 
    (last-n-digits 5) 
    drop-trailing-zeros))

(defn solve [^long limit]
  (loop [n 1
         res (long 1)]
    (if (> n limit)
      res
      (recur (inc n) (long (truncate (* (truncate n) res)))))))

(solve 9)
(solve 10)
(solve 20)

(solve 100000000)

(time (solve 1e12))
