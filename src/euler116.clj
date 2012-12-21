;; EULER #116
;; ==========
;; A row of five black square tiles is to have a number of its tiles 
;; replaced with coloured oblong tiles chosen from red (length two), 
;; green (length three), or blue (length four).
;;
;; If red tiles are chosen there are exactly seven ways this can be done.
;;
;;   RR---   -RR--   --RR-   ---RR   RRRR-   RR-RR   -RRRR   
;;
;; If green tiles are chosen there are three ways.
;; 
;;   GGG--   -GGG-   --GGG
;;
;; And if blue tiles are chosen there are two ways.
;;
;;   -BBBB   BBBB-
;;	
;;
;; Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of 
;; replacing the black tiles in a row measuring five units in length.
;;
;; How many different ways can the black tiles in a row measuring fifty 
;; units in length be replaced if colours cannot be mixed and at least one 
;; coloured tile must be used?
;; 
;; NOTE: This is related to problem 117.
;;

(ns euler116
  (:use [util.misc]
        [util.combinatorics]
        [util.binomial]))

(defn partitions 
  ([n] (partitions n n)) 
  ([n limit]
    (if (zero? n)
      [[]]
      (for [x (range (min n limit) 0 -1)
            p (partitions (- n x) x)]
        (cons x p)))))

(defn f [xs allowed]
  (and 
    (allowed (first xs))
    (every? (conj allowed 1) xs)))

(defn solve [n]
  (->>
    (partitions n)
    (filter #(or (f % #{2}) (f % #{3}) (f % #{4})))
    (map count-permutations)
    (reduce +)
    long))

(time (solve 50))
