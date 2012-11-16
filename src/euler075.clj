;; EULER #075
;; ==========
;; It turns out that 12 cm is the smallest length of wire that can be 
;; bent to form an integer sided right angle triangle in exactly one way,
;; but there are many more examples.
;; 
;;    12 cm: (3,4,5)
;;    24 cm: (6,8,10)
;;    30 cm: (5,12,13)
;;    36 cm: (9,12,15)
;;    40 cm: (8,15,17)
;;    48 cm: (12,16,20)
;;    
;; In contrast, some lengths of wire, like 20 cm, cannot be bent to form
;; an integer sided right angle triangle, and other lengths allow more 
;; than one solution to be found; for example, using 120 cm it is possible 
;; to form exactly three different integer sided right angle triangles.
;;))
;;    120 cm: (30,40,50), (20,48,52), (24,45,51)
;;
;; Given that L is the length of the wire, for how many values of L <= 
;; 1,500,000 can exactly one integer sided right angle triangle be formed?
;;
;; Note: This problem has been changed recently, please check that you are
;; using the right parameters.
;;

(ns euler075
  (:use [util.misc]
        [util.primes]))

(defn pythagorean-perimeters [limit]
  (let [sq (int (Math/sqrt limit))]
    (frequencies
      (for [n (range 1 sq 2)
            m (range 2 sq 2)
            :when (coprime? m n)       ;; Avoid duplicates
            p [(+ (abs (- (* m m) (* n n))) (* 2 m n) (* m m) (* n n))]
            k (range 1 (/ (inc limit) p))] ;; scaled triangles also give perims
        (* p k)))))

(defn solve [n]
  (count (filter #(= 1 (second %)) (pythagorean-perimeters n))))

(time (solve 1500000))
