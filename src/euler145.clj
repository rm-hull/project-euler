;; EULER #145
;; ==========
;; Some positive integers n have the property that the sum [ n + reverse(n) ] 
;; consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 
;; 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and
;; 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).
;;
;; There are 120 reversible numbers below one-thousand.
;;
;; How many reversible numbers are there below one-billion (10^9)?
;;

(ns euler145
  (use [util.misc]
       [util.palindromes]))

;(defmacro is-char [pred posn s] `(~pred (- (int (~posn ~s)) 48)))
;
;(def powers-of-10
;  (iterate (partial * 10) 1))
;
;(defn check-digits? [num-digits pred n]
;  (->> (str n)
;       (take num-digits)
;       (every? pred)))
;
;(defn is-reversible? [^long n]
;;  (let [xs (digits n)
;        odd-digits #{\1 \3 \5 \7 \9}
;        f (partial check-digits? 2 odd-digits)]
;    (->> (reverse xs)
;         (map + xs)
;         (map * powers-of-10)
;         (reductions +)
;         (every? f))))

(defn is-reversible? [^long n]
  (let [sum (+ n (reverse-digits n))]
    (->> sum str seq (every? #{\1 \3 \5 \7 \9}))))


(defn calc [coll]
  ;(let [starts-with-even-digit (fn [n] (-> n str (.charAt 0) #{\2 \4 \6 \8}))]
    (->> coll
         ;(filter starts-with-even-digit)
         (filter is-reversible?)
         count
         (* 2)));)

(defn solve [limit batch-size]
  (reduce + (pmap calc (partition-all batch-size (range 1 limit 2)))))

(time (solve 1000000000 50000))
