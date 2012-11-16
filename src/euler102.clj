;; EULER #102
;; ==========
;; Three distinct points are plotted at random on a Cartesian plane, for 
;; which -1000 <= x, y <= 1000, such that a triangle is formed.
;;
;; Consider the following two triangles:
;; 
;;    A(-340,495), B(-153,-910), C(835,-947)
;;
;;    X(-175,41), Y(-421,-714), Z(574,-645)
;;
;; It can be verified that triangle ABC contains the origin, whereas 
;; triangle XYZ does not.
;; 
;; Using 'data/triangles.txt', a 27K text file containing the co-ordinates
;; of one thousand "random" triangles, find the number of triangles for 
;; which the interior contains the origin.
;; 
;; NOTE: The first two examples in the file represent the triangles in the 
;; example given above.
;;

(ns euler102)

(defn in-triangle? [pt tri]
  (let [f (fn [[px py] [ax ay] [bx by]] 
            (- (* (- py ay) (- bx ax)) 
               (* (- px ax) (- by ay))))
        ab (f pt (:a tri) (:b tri))
        ca (f pt (:c tri) (:a tri))
        bc (f pt (:b tri) (:c tri))]
    (if 
      (and 
        (pos? (* ab bc))
        (pos? (* bc ca))) true)))

(defn parse-nums [s]
  (let [coords (map read-string (clojure.string/split s #","))
        [ax ay bx by cx cy] coords]
    {:a [ax ay] :b [bx by] :c [cx cy]}))

(defn get-data [fname]
  (map parse-nums (clojure.string/split-lines (slurp fname))))

(defn solve [fname]
  (let [data (get-data fname)]
    (count (filter #(in-triangle? [0 0] %) data))))

(time (solve "data/triangles.txt"))
