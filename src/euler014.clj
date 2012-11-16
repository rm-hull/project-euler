;; EULER #014
;; ==========
;; The following iterative sequence is defined for the set of positive 
;; integers:
;;
;;    n --> n/2 (n is even)
;;    n --> 3n + 1 (n is odd)
;;
;; Using the rule above and starting with 13, we generate the following
;;sequence:
;;
;;     13  40  20  10  5  16  8  4  2  1
;;
;; It can be seen that this sequence (starting at 13 and finishing at 1)
;; contains 10 terms. Although it has not been proved yet (Collatz 
;; Problem), it is thought that all starting numbers finish at 1.
;;
;; Which starting number, under one million, produces the longest chain?
;;
;; NOTE: Once the chain starts the terms are allowed to go above one 
;; million.
;;

(ns euler014)

(def collatz-length
  (memoize 
    (fn [n]
      (cond
        (<= n 0)  0
        (= n 1)   1
        (even? n) (inc (collatz-length (/ n 2)))
        :else     (inc (collatz-length (inc (* 3 n))))))))

(defn to-tuple [n]
  [n (collatz-length n)])

(defn maxf [a b]
  (if (> (second a) (second b)) a b))

(defn solve [n]
  (first (reduce maxf (map to-tuple (range 1 n)))))

(time (solve 1000000))
