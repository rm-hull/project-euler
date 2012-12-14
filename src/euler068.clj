;; EULER #068
;; ==========
;;
;; Consider the following "magic" 3-gon ring, filled with the numbers 1 
;; to 6, and each line adding to nine.
;;
;;    [see http://projecteuler.net/project/images/p_068_1.gif]
;;
;; Working clockwise, and starting from the group of three with the 
;; numerically lowest external node (4,3,2 in this example), each solution
;; can be described uniquely. For example, the above solution can be 
;; described by the set: 4,3,2; 6,2,1; 5,1,3.
;;
;; It is possible to complete the ring with four different totals: 9, 10, 
;; 11, and 12. There are eight solutions in total.
;;
;;  Total      Solution Set
;;  =====   ===================
;;    9     4,2,3; 5,3,1; 6,1,2
;;    9     4,3,2; 6,2,1; 5,1,3
;;    10    2,3,5; 4,5,1; 6,1,3
;;    10    2,5,3; 6,3,1; 4,1,5
;;    11    1,4,6; 3,6,2; 5,2,4
;;    11    1,6,4; 5,4,2; 3,2,6
;;    12    1,5,6; 2,6,4; 3,4,5
;;    12    1,6,5; 3,5,4; 2,4,6
;; 
;; By concatenating each group it is possible to form 9-digit strings; the
;; maximum string for a 3-gon ring is 432621513.
;; 
;; Using the numbers 1 to 10, and depending on arrangements, it is possible
;; to form 16- and 17-digit strings. What is the maximum 16-digit string for
;; a "magic" 5-gon ring?
;;
;;    [see http://projecteuler.net/project/images/p_068_2.gif]
;;

(ns euler068
  (:use [util.combinatorics]
        [util.misc]))

(def three-gon-ring
  (for [[a b c d e f] (permutations (range 1 7))
        :when (= (+ a b c)
                 (+ d c e)
                 (+ f e b))]
    { :total (+ a b c) 
      :solution [[a b c] [d c e] [f e b]] }))

(def five-gon-ring
  (for [[a b c d e f g h i j] (permutations (range 1 11))
        :when (= (+ a b c)
                 (+ d c e)
                 (+ f e g)
                 (+ h g i)
                 (+ j i b))]
    { :total (+ a b c) 
      :solution [[a b c] [d c e] [f e g] [h g i] [j i b]] }))

(defn first-node [ns]
  (get-in ns [:solution 0 0]))

(defn lowest-external-node [solution-set]
  (->> 
    solution-set
    (map first-node)
    (reduce min)))

(defn unique-solution-sets [solution-set]
  (let [n (lowest-external-node solution-set)]
    (filter #(= n (first-node %)) solution-set)))

(defn solution-sets [xs]
  (let [g (group-by :total xs)]
    (->> 
      (vals g)
      (map unique-solution-sets)
      (apply concat))))

(defn solve [ring filter-pred]
  (->>
    (solution-sets ring)
    (map (comp (partial apply str) flatten :solution))
    (filter filter-pred)
    (map read-string)
    (reduce max)))

(time (solve three-gon-ring identity))

(time (solve five-gon-ring #(= 16 (count %))))

