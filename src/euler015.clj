;; EULER #015
;; ==========
;; Starting in the top left corner of a 2x2 grid, there are 6 routes 
;; (without backtracking) to the bottom right corner.
;;
;; How many routes are there through a 2020 grid?
;;
;; 1 = Down / 0 = Across
;;
;; 0011   3
;; 0101   5 
;; 0110   6
;; 1001   9
;; 1010  10 
;; 1100  12
;;

(ns euler015
  (:use [util.combinatorics]))

(defn to-binary [n]
  (if (zero? n)
    (list 0)
    (loop [n n
           bits nil]
      (if (zero? n)
        bits
        (recur (quot n 2) (cons (rem n 2) bits))))))

(defn count-bits [n]
  (loop [n n
         acc 0]
    (if (zero? n) 
      acc
      (recur (bit-shift-right n 1) (+ acc (mod n 2))))))

(def count-bits
  (memoize 
    (fn [n]
      (if (zero? n)
        0
        (+ (count-bits (bit-shift-right n 1) (mod n 2)))))))

(defn slow-solve [n]
  (let [start (dec (Math/pow 2 n)) 
        end   (quot (Math/pow 4 n) 2)] 
    (* 2 (count (filter #(= % n) (map #(count-bits %) (range start end)))))))

(defn solve [n]
  (let [bits (concat (repeat (dec n) 0) (repeat n 1))]
    (* 2 (count (lex-permutations bits)))))

(def factorial
  (memoize
    (fn [n]
      (if (= n 1)
        1
        (* n (factorial (dec n)))))))

;; Pascal's triangle / combinatorics

(defn solve [k]
  (let [n (* k 2)]
    (/ (factorial n)
       (* (factorial k) (factorial (- n k))))))

(time (solve 20))
