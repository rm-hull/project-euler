;; EULER #114
;; ==========
;; A row measuring seven units in length has red blocks with a minimum 
;; length of three units placed on it, such that any two red blocks 
;; (which are allowed to be different lengths) are separated by at least 
;; one black square. There are exactly seventeen ways of doing this.
;;
;;    -------   RRR----   -RRR---
;;
;;    --RRR--   ---RRR-   ----RRR
;;
;;    RRR-RRR   RRRR---   -RRRR--
;;
;;    --RRRR-   ---RRRR   RRRRR--
;;
;;    -RRRRR-   --RRRRR   RRRRRR-
;;
;;    -RRRRRR   RRRRRRR
;;
;; How many ways can a row measuring fifty units in length be filled?
;;
;; NOTE: Although the example above does not lend itself to the possibility,
;; in general it is permitted to mix block sizes. For example, on a row 
;; measuring eight units in length you could use red (3), black (1), and
;; red (4).
;;

(ns euler114
  (:use [util.partition]))

(defn f [xs]
  (let [d (distinct xs)]
    (or 
      (and 
        (every? #(>= % 3) (butlast d))
        (= (last d) 1))  
      (and 
        (not= (first d) 2)
        (= (count d) 1))
       )))

(defn solve [n]
  (->>
    (partitions n)
    (filter f)
    ;(map count-permutations)
    ;(reduce +)
    ;long
    ))

(time (solve 7))

(partitions 7)

(distinct '(4 1  1 1))

; 16475640049
; 128407247

(count-partitions '(7))
