;; EULER #117
;; ==========
;; Using a combination of black square tiles and oblong tiles chosen from:
;; red tiles measuring two units, green tiles measuring three units, and 
;; blue tiles measuring four units, it is possible to tile a row measuring
;; five units in length in exactly fifteen different ways.
;;
;;    -----   RR---   -RR--   --RR-
;;
;;    ---RR   RRRR-   RR-RR   -RRRR
;;
;;    GGG--   -GGG-   --GGG   RRGGG
;;
;;    GGGRR   BBBB-   -BBBB
;;
;; How many ways can a row measuring fifty units in length be tiled?
;;
;; NOTE: This is related to problem 116.
;;

(ns euler117
  (:use [util.partition]))

(defn f [xs allowed]
    (every? (conj allowed 1) xs))

(defn solve [n]
  (->>
    (partitions n)
    (filter #(f % #{2 3 4}))
    (map count-permutations)
    (reduce +)
    long))

(time (solve 50))
