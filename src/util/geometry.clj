(ns util.geometry)

(defn cartesian->polar [[^double x ^double y]]
  { :r (Math/sqrt (+ (* x x) (* y y)))
    :theta (Math/atan2 y x) })

(defn minkowski-sum
  ([coll] (reduce minkowski-sum coll)) 
  ([a b]
    (for [[^double u ^double v] a
          [^double x ^double y] b]
      [(+ u x) (+ v y)])))

(defn x-angle 
  "Calculates the angle OP makes with the X axis"
  [[ox oy] [px py]]
  (let [^double opp (- py oy)
        ^double adj (- px ox)]
    (Math/atan2 opp adj)))

(defn cross 
  "2D cross product of OA and OB vectors, i.e. z-component of their 3D 
   cross product. Returns a positive value, if OAB makes a counter-clockwise
   turn, negative for clockwise turn, and zero if the points are collinear."
  [[^double ox ^double oy] [^double ax ^double ay] [^double bx ^double by]]
    (- 
      (* (- ax ox) (- by oy)) 
      (* (- ay oy) (- bx ox))))

(defn- reduce-down 
  "While the vector contains at least two points and the sequence of last
   two points and the point P does not make a counter-clockwise turn, then
   remove the last point"
  [pts p]
  (loop [pts pts]
    (let [sz (count pts)]
      (if (and 
            (>= sz 2)
            (<= (cross 
                  (nth pts (- sz 2))
                  (nth pts (- sz 1))
                  p) 0))
        (recur (pop pts))
        (conj pts p)))))

(defn- build-hull [points]
  (loop [res []
         v (vec points)]
    (if (empty? v)
      (pop (vec res))
      (recur 
        (reduce-down res (first v))
        (subvec v 1)))))

(defn monotone-chain-convex-hull 
  "Andrew's monotone chain convex hull algorithm constructs the convex hull
   of a set of 2-dimensional points in O(n log n) time. It does so by first
   sorting the points lexicographically (first by x-coordinate, and in case
   of a tie, by y-coordinate), and then constructing upper and lower hulls
   of the points in O(n) time. An upper hull is the part of the convex hull,
   which is visible from the above. It runs from its rightmost point to the
   leftmost point in counterclockwise order. Lower hull is the remaining 
   part of the convex hull."
  [points]
  (let [sorted (sort compare points)
        lower (build-hull sorted)
        upper (build-hull (reverse sorted))]
    (concat lower upper)))
