;; EULER #179
;; ==========
;; Find the number of integers 1 < n < 10^7, for which n and n + 1 have 
;; the same number of positive divisors. For example, 14 has the positive
;; divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.

(ns euler179)

(defn divisors-sieve [limit]
  (let [sqrt (int (Math/sqrt limit))
        size (inc limit)
        arr (int-array size)]
    (doseq [i (range 2 (inc (int (Math/sqrt limit))))]
      (let [j (* i i)]
        (aset-int arr j 1)
        (doseq [k (range (int j) size i)]
          (aset-int arr k (+ (aget arr k) 2)))))
    arr))

(defn solve [limit]
  (let [sieve (divisors-sieve limit)
        consec (fn [^long i] (= (aget sieve i) (aget sieve (inc i))))]
    (->> (range 2 limit)
         (map consec)
         (reduce +))))

;(time (solve 10000000))

