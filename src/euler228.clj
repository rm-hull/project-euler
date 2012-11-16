;; EULER #228
;; ==========
;; Let S[n] be the regular n-sided polygon, or shape, whose vertices 
;; v[k] (k = 1,2,...,n) have coordinates:
;;
;;    x[k] = cos( 2k-1/n x 180°)
;;    y[k] = sin( 2k-1/n x 180°)
;;
;; Each S[n] is to be interpreted as a filled shape consisting of all 
;; points on the perimeter and in the interior.
;;
;; The Minkowski sum, S+T, of two shapes S and T is the result of adding 
;; every point in S to every point in T, where point addition is performed
;; coordinate-wise: (u, v) + (x, y) = (u+x, v+y).
;; 
;; For example, the sum of S[3] and S[4] is the six-sided shape shown in
;; http://projecteuler.net/project/images/p_228.png
;;
;; How many sides does S[1864] + S[1865] + ... + S[1909] have?
;;

(ns euler228
  (:use ;[incanter core stats charts datasets]
        [util.geometry]))

(defn create-polygon [n]
  (for [^long k (range 1 (inc n))
        :let [c (/ (* Math/PI (dec (+ k k))) n)]] 
   [ (Math/cos c) (Math/sin c) ]))

(defn draw-shape [polygon]
  (with-data (dataset [:x :y] polygon)
    (view (scatter-plot ($ :x) ($ :y)))))

(defn solve [lower-limit upper-limit]
  (->> (range lower-limit (inc upper-limit))
       (map create-polygon) 
       (reduce (comp monotone-chain-convex-hull minkowski-sum))
       distinct
       count))

(time (solve 1864 1909))

; target = 86226
