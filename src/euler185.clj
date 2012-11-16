;; EULER #185
;; ==========
;; The game Number Mind is a variant of the well known game Master Mind.
;;
;; Instead of coloured pegs, you have to guess a secret sequence of digits.
;; After each guess you're only told in how many places you've guessed the
;; correct digit. So, if the sequence was 1234 and you guessed 2036, you'd
;; be told that you have one correct digit; however, you would NOT be told
;; that you also have another digit in the wrong place.
;;
;; For instance, given the following guesses for a 5-digit secret sequence,
;;
;;    90342 ;2 correct
;;    70794 ;0 correct
;;    39458 ;2 correct
;;    34109 ;1 correct
;;    51545 ;2 correct
;;    12531 ;1 correct
;;
;; The correct sequence 39542 is unique.
;;
;; Based on the following guesses,
;;
;;    5616185650518293 ;2 correct
;;    3847439647293047 ;1 correct
;;    5855462940810587 ;3 correct
;;    9742855507068353 ;3 correct
;;    4296849643607543 ;3 correct
;;    3174248439465858 ;1 correct
;;    4513559094146117 ;2 correct
;;    7890971548908067 ;3 correct
;;    8157356344118483 ;1 correct
;;    2615250744386899 ;2 correct
;;    8690095851526254 ;3 correct
;;    6375711915077050 ;1 correct
;;    6913859173121360 ;1 correct
;;    6442889055042768 ;2 correct
;;    2321386104303845 ;0 correct
;;    2326509471271448 ;2 correct
;;    5251583379644322 ;2 correct
;;    1748270476758276 ;3 correct
;;    4895722652190306 ;1 correct
;;    3041631117224635 ;3 correct
;;    1841236454324589 ;3 correct
;;    2659862637316867 ;2 correct
;;
;; Find the unique 16-digit secret sequence.
;;

(ns euler185
  (:use [util.misc]
        [util.combinatorics]))

(def guesses
  { 5616185650518293 2 
    3847439647293047 1 
    5855462940810587 3 
    9742855507068353 3 
    4296849643607543 3 
    3174248439465858 1 
    4513559094146117 2 
    7890971548908067 3 
    8157356344118483 1 
    2615250744386899 2 
    8690095851526254 3 
    6375711915077050 1 
    6913859173121360 1 
    6442889055042768 2 
    2321386104303845 0 
    2326509471271448 2 
    5251583379644322 2 
    1748270476758276 3 
    4895722652190306 1 
    3041631117224635 3 
    1841236454324589 3 
    2659862637316867 2 })

;(def guesses
;  { 90342 2
;    70794 0
;    39458 2
;    34109 1
;    51545 2
;    12531 1 })

(sort-by val guesses)

(combinations (digits 51545) 2)

(defn combinations-count [n k]
  (let [n! (nth factorial-seq (dec n))
        k! (nth factorial-seq (dec k))
        n-k! (nth factorial-seq (dec (- n k)))]
    (/ n! (* k! n-k!))))

(combinations-count 5 2)
(combinations-count 16 2)

(take 6 factorial-seq)


;120 / 2 * 6

(nth factorial-seq 4)
