;; EULER #370
;; ==========
;; Let us define a geometric triangle as an integer sided triangle with
;; sides a <= b <= c so that its sides form a geometric progression, i.e. 
;; b^2 = a · c . 
;;
;; An example of such a geometric triangle is the triangle with sides
;; a = 144, b = 156 and c = 169.
;; 
;; There are 861805 geometric triangles with perimeter <= 10^6 .
;;
;; How many geometric triangles exist with perimeter <= 2.5x10^13 ?)
;;

(ns euler370
  (:use [util.misc]))

(defn geometric-triangles [f b]
  (let [d (divisors b)
        bsq (* b b)]
    (map #(f % b (/ bsq %)) d)))

(def perimeter (partial geometric-triangles +))

(def sides (partial geometric-triangles vector))



(take 100 (mapcat sides squares))

(count 
  (take-while  #(<= % 1000000)
                   (map perimeter (iterate inc 1))))

(divisors 156)

(perimeter 156)

(sides 156)

(double (iexpt (/ 1000000 3) 2))

(take 1001 squares)
