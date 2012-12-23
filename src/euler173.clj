;; EULER #173
;; ==========
;; We shall define a square lamina to be a square outline with a square
;; "hole" so that the shape possesses vertical and horizontal symmetry.
;; For example, using exactly thirty-two square tiles we can form two 
;; different square laminae:
;;
;;     ######     #########
;;     ######     #       #
;;     ##  ##     #       #
;;     ##  ##     #       #
;;     ######     #       #
;;     ######     #       #
;;                #       #
;;                #       #
;;                #########
;;
;; With one-hundred tiles, and not necessarily using all of the tiles at 
;; one time, it is possible to form forty-one different square laminae.
;;
;; Using up to one million tiles how many different square laminae can be
;; formed?
;;

(ns euler173
  (:use [util.misc]))

(defn square-laminae [^long start]
  (->>
    (iterate (partial + 2) start)
    (map (partial * 4))))

(defn solve [^long n]
  (letfn [(f [i] (take-while #(<= % n) (reductions + (square-laminae i))))]
    (->>
      (range 2 (inc (quot n 4)))
      (mapcat f)
      count)))

(time (solve 1000000))
