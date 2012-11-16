;; EULER #037
;; ==========
;; The number 3797 has an interesting property. Being prime itself, it is
;; possible to continuously remove digits from left to right, and remain
;; prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
;; right to left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable from
;; left to right and right to left.
;;
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
;;

(ns euler037
  (:use [util.primes]))

(defn right-to-left [n]
  (take-while pos?
              (map #(int (/ n %)) 
                   (reductions * 1 (repeat 10)))))

(defn left-to-right [n]
  (let [s (str n)
        len (count s)
        substr (fn [idx] (if (< idx len) (Long/valueOf (.substring s idx))))
        not-nil? (fn [x] (not (nil? x)))]
    (take-while not-nil? (map substr (iterate inc 0)))))

(defn is-truncatable? [n]
  (every? 
    is-prime? 
    (distinct 
      (sort 
        (concat 
          (left-to-right n)
          (right-to-left n))))))

(defn solve []
  (reduce + (take 11 (filter is-truncatable? (drop-while #(<= % 7) primes)))))

(time (solve))
