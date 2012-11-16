;; EULER #204
;; ==========
;; A Hamming number is a positive number which has no prime factor larger
;; than 5. So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10,
;; 12, 15. There are 1105 Hamming numbers not exceeding 10^8.
;;
;; We will call a positive number a generalised Hamming number of type n, if
;; it has no prime factor larger than n. Hence the Hamming numbers are the 
;; generalised Hamming numbers of type 5.
;;
;; How many generalised Hamming numbers of type 100 are there which don't
;; exceed 10^9?
;;

(ns euler204
  (:use [util.primes]))

(defn smerge [xs ys]
  (lazy-seq
    (let [^long x (first xs)
          ^long y (first ys)]
      (cond 
        (< x y) (cons x (smerge (rest xs) ys))
        (> x y) (cons y (smerge xs (rest ys)))
        :else   (cons x (smerge (rest xs) (rest ys)))))))

(defn smerge-n [xs ys & more] 
  (loop [res  (smerge xs ys)
         more more]
    (if (empty? more)
      res
      (recur 
        (smerge res (first more))
        (rest more)))))

(defn scale [^long n ks] 
  (map #(* n %) ks))
 
(defn streams [limit xs] 
  (map #(scale % xs) (reverse (primes-range 2 limit))))

(defn k-smooth-seq [k]
  (lazy-seq
    (cons 1 (apply smerge-n 
              (streams k (k-smooth-seq k))))))

(def hamming (k-smooth-seq 5))

(defn solve [n limit]
  (->> (k-smooth-seq n)
       (take-while #(<= % limit))
       count))

;(time (solve 5 1e8))

;(time (solve 100 1e9))



