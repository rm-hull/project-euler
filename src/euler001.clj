;; EULER #001
;; ==========
;; Add all the natural numbers below one thousand that
;; are multiples of 3 or 5.
;;

(ns euler001)

(defn solve [n]
  (letfn [(f [m r] (range m r m))]
    (reduce + (distinct (concat (f 3 n) (f 5 n))))))

(time (solve 1000))

