;; EULER #206
;; ==========
;; Find the unique positive integer whose square has the form 
;; 1_2_3_4_5_6_7_8_9_0, where each "_" is a single digit.
;;

(ns euler206
  (:use [util.misc]))

(defn squincrementer [^long n]
  (let [r (rem n 100)]
    (cond 
      (= r 30) (+ n 40)
      (= r 70) (+ n 60)
      :else    (inc n))))

(defn squares-from [^long root]
  (map square (iterate squincrementer root)))

(defn splinter [^long n] 
  (take-nth 2 (digits n)))

(defn solve []
  (let [start 1020304050607080900
        end   1929394959697989990
        sqrt  (int (Math/sqrt start))
        dig   (list 1 2 3 4 5 6 7 8 9 0)]
    (->> (squares-from sqrt)
         (take-while #(< % end))
         (filter #(= dig (splinter %)))
         first
         Math/sqrt
         long)))

(time (solve))
