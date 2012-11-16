;; EULER #074
;; ==========
;; The number 145 is well known for the property that the sum of the factorial
;; of its digits is equal to 145:
;;
;;    1! + 4! + 5! = 1 + 24 + 120 = 145
;;
;; Perhaps less well known is 169, in that it produces the longest chain of
;; numbers that link back to 169; it turns out that there are only three 
;; such loops that exist:
;;
;;    169 --> 363601 --> 1454 --> 169
;;    871 --> 45361 --> 871
;;    872 --> 45362 --> 872
;;
;; It is not difficult to prove that EVERY starting number will eventually
;; get stuck in a loop. For example,
;;
;;    69 --> 363600 --> 1454 --> 169 --> 363601 (--> 1454)
;;    78 --> 45360 --> 871 --> 45361 (--> 871)
;;    540 --> 145 (--> 145)
;;
;; Starting with 69 produces a chain of five non-repeating terms, but the 
;; longest non-repeating chain with a starting number below one million is
;; sixty terms.
;;
;; How many chains, with a starting number below one million, contain 
;; exactly sixty non-repeating terms?
;;

(ns euler074
  (:use [util.misc]))

(def fac-digits
  (->> (take 10 factorial-seq)
       (map int)
       vec))

;(defn factorial-digits-chain [n]
;  (lazy-seq 
;    (cons n (factorial-digits-chain (sum-of fac-digits n)))))
;
;(defn count-repeating-terms [n max-terms]
;  (->> (factorial-digits-chain n)
;       (take max-terms)
;       distinct 
;       count)) 
;
;(defn count-repeating-terms [n]
;  (loop [fact-seq (factorial-digits-chain n)
;         non-rpt-terms #{}]
;    (if (non-rpt-terms (first fact-seq))
;      (count non-rpt-terms)
;      (recur 
;        (rest fact-seq)
;        (conj non-rpt-terms (first fact-seq))))))

(def count-repeating-terms
  (memoize
    (fn [n]
      (let [known-loops { 1 1, 2 1, 145 1, 169 3, 871 2, 872 2, 40585 1 }]
        (if-let [num-terms (get known-loops n)]
          num-terms 
          (inc (count-repeating-terms (sum-of fac-digits n))))))))

(defn solve [n num-terms]
  (->> (range 1 n)
       (filter #(= num-terms (count-repeating-terms %)))
       count))

(time (solve 1000000 60))
