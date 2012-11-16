;; EULER #092
;; ==========
;; A number chain is created by continuously adding the square of the
;; digits in a number to form a new number until it has been seen before.
;; 
;; For example,
;; 
;;   44 --> 32 -->  13 --> 10 -->  1 --> 1
;;   85 --> 89 --> 145 --> 42 --> 20 --> 4 --> 16 --> 37 --> 58 --> 89
;; 
;; Therefore any chain that arrives at 1 or 89 will become stuck in an 
;; endless loop. What is most amazing is that EVERY starting number will 
;; eventually arrive at 1 or 89.
;;
;; How many starting numbers below ten million will arrive at 89?
;;

(ns euler092)

(defn char-num [c]
  (- (int c) 48))

(def next-in-chain
  (memoize
    (fn [n]
      (reduce + (map #(* % %) (map char-num (str n)))))))

(defn last-in-chain [n]
  (cond 
    (= n 1) 1
    (= n 89) 89
    :else (recur (next-in-chain n))))

(defn number-chain [n]
  (lazy-seq
    (cond 
      (= n 1) (seq [1])
      (= n 89) (seq [89])
      :else (cons n (number-chain (next-in-chain n))))))

(defn last-in-chain [n]
  last (number-chain n))

(defn solve [n]
  (count (filter #(= % 89) (take n (map last-in-chain (iterate inc 1))))))

(time (solve 10000000))


