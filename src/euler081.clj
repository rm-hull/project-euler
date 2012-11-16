;; EULER #081
;; ==========
;; In the 5 by 5 matrix below, the minimal path sum from the top left to
;; the bottom right, by only moving to the right and down, is indicated 
;; in bold red and is equal to 2427.
;;
;;    131 673 234 103  18
;;    201  96 342 965 150
;;    630 803 746 422 111
;;    537 699 497 121 956
;;    805 732 524  37 331
;;
;; Find the minimal path sum, in 'data/80-matrix.txt', a 31K text file 
;; containing a 80 by 80 matrix, from the top left to the bottom right 
;; by only moving right and down.
;;

(ns euler081
  (:use [clojure.string :only (split-lines split)]))

(defn load-data [fname]
  (letfn [(parse-nums [s] (vec (map #(Long/parseLong %) (split s #"\W"))))]
    (vec (map parse-nums (split-lines (slurp fname))))))

(defn min-pairs [xs]
  (map (partial apply min) (partition-all 2 1 xs)))

(defn calc [triangle]
  (loop [data (rest triangle)
         accum (first triangle)]
    (if (empty? data)
      accum   
      (recur 
        (rest data)
        (map + (min-pairs accum) (first data))))))   

(defn diagonal-slice [data f start-y]
  (for [y (range start-y -1 -1)
        :let [x (- start-y y)]]
    (f data x y)))

(defn get-at [data x y]
  (nth (nth data y []) x 99999))

(defn count-slices [data]
  (range 0 
         (+ 
           (count (first data)) 
           (count data) 
           -1)))

(defn transform [data]
  (->> data
       count-slices
       (map (partial diagonal-slice data get-at))
       reverse))

(defn solve [fname]
  (->> fname
       load-data
       transform
       calc))

(time (solve "data/5-matrix.txt"))

(time (solve "data/80-matrix.txt"))
