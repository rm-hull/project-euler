;; EULER #076
;; ==========
;; It is possible to write five as a sum in exactly six different ways:
;;
;;  4 + 1
;;  3 + 2
;;  3 + 1 + 1
;;  2 + 2 + 1
;;  2 + 1 + 1 + 1
;;  1 + 1 + 1 + 1 + 1
;;
;; How many different ways can one hundred be written as a sum of at 
;; least two positive integers?
;;


(defn count-partitions[c v]
  (let [f (first c)]
    (if (= f 1) 
      1
      (reduce + 
        (for [n (range 0 (inc (quot v f)))]
          (count-partitions (rest c) (- v (* n f))))))))

(defn solve [n]
  (count-partitions (range (dec n) 0 -1) n))

(time (solve 100))


(defn pentagonal-num [n] 
  (/ (* n (dec (* 3 n))) 2))

(def partition-seq
  (let [pos-pentagonal (map pentagonal-num (iterate inc 1))
        neg-pentagonal (map pentagonal-num (iterate dec -1))]
    (interleave pos-pentagonal neg-pentagonal)))

(defn partition-fn [n]
  (cond
    (neg? n)  0
    (= n 1)   1
    (odd? n)  (pentagonal-num (quot (inc n) 2))
    (even? n) (pentagonal-num (quot (inc n) -2))))

(def partition-seq2
   (map partition-fn (iterate inc 1)))

(take 30 partition-seq2)

(take 30 partition-seq)
