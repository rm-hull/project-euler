;; EULER #038
;; ==========
;; Take the number 192 and multiply it by each of 1, 2, and 3:
;;
;;    192 x 1 = 192
;;    192 x 2 = 384
;;    192 x 3 = 576
;;
;; By concatenating each product we get the 1 to 9 pandigital, 192384576.
;; We will call 192384576 the concatenated product of 192 and (1,2,3)
;;
;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3,
;; 4, and 5, giving the pandigital, 918273645, which is the concatenated 
;; product of 9 and (1,2,3,4,5).
;;
;; What is the largest 1 to 9 pandigital 9-digit number that can be formed 
;; as the concatenated product of an integer with (1,2, ... , n) where 
;; n > 1?
;;
 
(ns euler038
  (:use [util.misc]
        [util.combinatorics]))
 
(defn pandigital-seq [n]
  (permutations (range n 0 -1)))
 
(defn is-concatenated-product [xs n]
  (let [seed (to-number (take n xs))
        generated-num (read-string (subs (apply str (take 4 (reductions + (repeat seed)))) 0 9))
        pandigital-num (to-number xs)]
    (= generated-num pandigital-num)))
 
(defn solve [n]
  (first
    (for [a (pandigital-seq n)
          b (range 2 5)
          :when (is-concatenated-product a b)]
      (to-number a))))
 
(time (solve 9))
