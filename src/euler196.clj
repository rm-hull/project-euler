;; EULER #196
;; ==========
;; Build a triangle from all positive integers in the following way:
;;
;;   1
;;   2  3
;;   4  5  6
;;   7  8  9 10
;;  11 12 13 14 15
;;  16 17 18 19 20 21
;;  22 23 24 25 26 27 28
;;  29 30 31 32 33 34 35 36
;;  37 38 39 40 41 42 43 44 45
;;  46 47 48 49 50 51 52 53 54 55
;;  56 57 58 59 60 61 62 63 64 65 66
;;  . . .
;;
;; Each positive integer has up to eight neighbours in the triangle.
;;
;; A set of three primes is called a prime triplet if one of the three primes
;; has the other two as neighbours in the triangle.
;;
;; For example, in the second row, the prime numbers 2 and 3 are elements of
;; some prime triplet.
;;
;; If row 8 is considered, it contains two primes which are elements of some
;; prime triplet, i.e. 29 and 31. If row 9 is considered, it contains only
;; one prime which is an element of some prime triplet: 37.
;;
;; Define S(n) as the sum of the primes in row n which are elements of any 
;; prime triplet. Then S(8)=60 and S(9)=37.
;;
;; You are given that S(10000)=950007619.
;;
;; Find S(5678027) + S(7208785).
;;

(ns euler196
  (:use [util.primes]
        [util.misc]))

(defn get-row [n]
  (let [start (inc (triangle (dec n)))
        end   (+ start n)]
   (range start end))) 

(defn primes-in-row [n]
  (let [start (inc (triangle (dec n)))
        end   (+ start n)]
   (->> (primes-after (dec start))
        (take-while #(< % end))))) 

(defn odd-neighbours-of [n]
  (let [row-of (fn [a] (int (Math/ceil (triangle-root a))))
        r      (row-of n)
        flt    (fn [a] (= 1 (abs (- r (row-of a)))))
        poss-n (if (odd? r)
                 [(- n r -1) (+ n r -1) (+ n r  1)]
                 [(- n r)    (- n r -2) (+ n r)])]
    (filter flt poss-n)))

(defn get-odd-triplets [a]
  (for [b (odd-neighbours-of a)
        c (odd-neighbours-of b)
        :when (not= a c)]
    [a b c]))

(defn is-part-of-a-prime-triplet? [n]
  (->> (get-odd-triplets n)
       (some #(every? is-prime? %))))

(is-part-of-a-prime-triplet? 35)

(defn s [n]
  (->> (primes-in-row n)
       (filter is-part-of-a-prime-triplet?)
       (reduce +)))

(+ 
  (s 5678027)
  (s 7208785))

((juxt first last) (get-row  9998))
((juxt first last) (get-row  9999))
((juxt first last) (get-row 10000))
((juxt first last) (get-row 10001))
((juxt first last) (get-row 10002))
((juxt first last) (get-row 10003))
((juxt first last) (get-row 10004))


(drop 9995 (get-row  9998))
(drop 9995 (get-row  9999))
(drop 9995 (get-row 10000))
(drop 9995 (get-row 10001))
(drop 9995 (get-row 10002))
(drop 9995 (get-row 10003))
(drop 9995 (get-row 10004))

(map #(every? is-prime? %) (get-odd-triplets 49995811))
(map #(every? is-prime? %) (get-odd-triplets 49997501))
(map #(every? is-prime? %) (get-odd-triplets 49998209))
(map #(every? is-prime? %) (get-odd-triplets 49999441))

(get-odd-triplets 49995041)

(odd-neighbours-of 41)


(primes-in-row 9)
(get-row 9)

(range 2 (inc (Math/sqrt 50)))

(- 950007619 550004555)
(reduce + (get-row 10000))

(get-row 11)

(primes-in-row 10000)


(->> (list 9999 10000 10001 10002)
     (map #(hash-map % (vector (first (get-row %)) (last (get-row %))))))

(int (Math/ceil (triangle-root 49995000)))

(take 20 (primes-after 50005000))
