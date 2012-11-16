;; EULER #146
;; ==========
;; The smallest positive integer n for which the numbers n^2+1, n^2+3, n^2+7, 
;; n^2+9, n^2+13, and n^2+27 are consecutive primes is 10. The sum of all such
;; integers n below one-million is 1242490.
;;
;; What is the sum of all such integers n below 150 million?
;; 

(ns euler146
  (:use [util.primes]
        [util.misc]))



(defn solve [limit]
  ;(reduce +
    (for [n (range 10 limit 10)
          :let [n-sqr (* n n)
                s (map (partial + n-sqr) '(1 3 7 9 13 27))]
          :when (and
                  (= (mod (first s) 4) 1)
                  (every? is-prime? s))]
      n))
;)

(time (solve 1000000))

2
4
2
2
14

(take 6 (primes-after (dec 101)))

(take 20 (partition 6 1 primes))

(defn generate [n]
  (map (partial + n) '(0 2 6 8 12 26)))

(generate 101)

(defn solve [limit]
  (reduce + 
    (take-while #(< % limit)
      (for [p (partition 6 1 primes)
            :let [n-sqr (dec (first p))
                  s (map (partial + n-sqr) '(1 3 7 9 13 27))]
            :when (= p s)
            :let [n (int (Math/sqrt n-sqr))]
            :when (= (* n n) n-sqr)]
        n))))
              

(doc partition)

(take 30
(partition 6 1 primes)
)

(defn x [a b] (- b a))

(map  #(- % 100) '(101 103 107 109 113 127))

last digit of n   --> n^2
0                     0         y
1                     1         n
2                     4         n
3                     9         n
4                     6         n
5                     5         n
6                     6         n
7                     9         n
8                     4         n
9                     1         n


(* 99 99)
