;; EULER #085
;; ==========
;; By counting carefully it can be seen that a rectangular grid measuring
;; 3 by 2 contains eighteen rectangles:
;;
;;   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+
;;   |X| | |   |X|X| |   |X|X|X|   |X| | |   |X|X| |   |X|X|X|
;;   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+
;;   | | | |   | | | |   | | | |   |X| | |   |X|X| |   |X|X|X|
;;   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+   +-+-+-+
;;      6         4         2         3         2         1
;;
;; Although there exists no rectangular grid that contains exactly two 
;; million rectangles, find the area of the grid with the nearest solution.
;;

(ns euler085
  (:use [util.misc]))

(defn num-rectangles 
  "Optimized (* (triangle x) (triangle y))"
  [^long x ^long y]
  (bit-shift-right (* x y (inc x) (inc y)) 2))

(defn solve [n tolerance]
  (let [lower-limit (- n tolerance)
        upper-limit (+ n tolerance)
        search-space (int (Math/sqrt n))]
    (sort-by #(abs (- n (:nearest %)))
      (for [x (range 1 search-space)
            y (range x search-space)
            :let [nearest (num-rectangles x y)]
            :when (and 
                    (> nearest lower-limit)
                    (< nearest upper-limit))]
        { :nearest nearest :area (* x y) }))))

(time (solve 2000000 10))
