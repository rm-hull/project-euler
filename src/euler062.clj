;; EULER #062
;; ==========
;; The cube, 41063625 (345^3), can be permuted to produce two other cubes:
;; 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest
;; cube which has exactly three permutations of its digits which are also
;; cube.
;;
;; Find the smallest cube for which exactly five permutations of its digits
;; are cube.
;;

(ns euler062)

(defn sort-digits [n]
  (apply str (sort (str n))))

(defn solve [n]
  (loop [i 0 m {}]
    (let [k (sort-digits (* i i i))
          vs (cons i (get m k []))]
      (if (= n (count vs))
        (first 
          (sort 
            (map #(* % % %) vs)))
        (recur 
          (inc i)
          (assoc m k vs))))))
        
(time (solve 5))

