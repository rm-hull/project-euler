;; EULER #039
;; ==========
;; If p is the perimeter of a right angle triangle with integral length
;; sides, {a,b,c}, there are exactly three solutions for p = 120.
;; 
;;    {20,48,52}, {24,45,51}, {30,40,50}
;;
;; For which value of p <= 1000, is the number of solutions maximised?
;;

(ns euler039)

(defn is-rhs-triangle? [a b p]
  (let [c (- p a b)]
    (= (* c c) (+ (* a a) (* b b)))))

(defn get-triangles [p]
  (let [pinc (inc p)]
    {:perimeter p 
     :triangles (for [a (range 2 pinc)
                      b (range a (- pinc a))
                      :when (is-rhs-triangle? a b p)]
                  (vector a b (- p a b)))}))

(defn max-triangles [a b]
  (if (> (count (:triangles a)) (count (:triangles b))) a b))

(defn calc [coll]
  (reduce max-triangles (map get-triangles coll)))

(defn solve [batch-size perimeter]
  (let [rng (range 12 (inc perimeter))
        batches (partition-all batch-size rng)]
    (reduce max-triangles (pmap calc batches))))

(time (solve 20 1000))
