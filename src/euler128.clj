;; EULER #128
;; ==========
;;

(ns euler128
  (:use [util.misc]))

(defn calc-ring [n]
  (inc (count (take-while #(< % n) centred-hexagonal-numbers))))

(defn is-corner? [n]
  (if (= n 1)
    true
    (let [len    (dec (calc-ring n))
          offset (inc (centred-hexagon n))]
        (zero? (rem (- n offset) len)))))

(->> (take 40 integers)
     (filter is-corner?))

(take 20 centred-hexagonal-numbers)

(take 40 (map #(vector % (calc-ring %)) integers))

(centred-hexagon 2)
