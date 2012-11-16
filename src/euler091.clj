;; EULER #091
;; ==========
;; The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates
;; and are joined to the origin, O(0,0), to form ΔOPQ.
;;
;; There are exactly fourteen triangles containing a right angle that can
;; be formed when each co-ordinate lies between 0 and 2 inclusive; that is,
;; 0 <= x1, y1, x2, y2 <= 2.
;;
;; Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be 
;; formed?
;;

(defn get-length-squared ^long [[^long x1 ^long y1] [^long x2 ^long y2]]
  (let [width  (- x1 x2)
        height (- y1 y2)]
    (+ (* width width)
       (* height height))))

(defn is-rhs-triangle? [o p q]
  (cond
    (= o p) false
    (= o q) false
    (= p q) false
    :else 
      (let [op (get-length-squared o p)
            oq (get-length-squared o q)
            pq (get-length-squared p q)]
        (or 
          (= op (+ pq oq))
          (= oq (+ pq op))
          (= pq (+ op oq))))))

(defn count-triangles [coll]
  (count (filter #(apply is-rhs-triangle? %) coll)))

(defn feature-space [n]
  (let [o [0 0]]
    (for [py (range 1 (inc n))
          qx (range 1 (inc n))
          px (range 0 (inc qx))
          qy (range 0 (inc py))]
      [o [px py] [qx qy]])))

(defn solve [n]
  (->> (feature-space n)
       (partition-all 10000)
       (pmap count-triangles)
       (reduce +)))

(time (solve 50))

