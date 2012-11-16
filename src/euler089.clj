;; EULER #089
;; ==========
;; The rules for writing Roman numerals allow for many ways of writing each
;; number (see http://projecteuler.net/about=roman_numerals). However, there
;; is always a "best" way of writing a particular number.
;;
;; For example, the following represent all of the legitimate ways of 
;; writing the number sixteen:
;;
;;    IIIIIIIIIIIIIIII
;;    VIIIIIIIIIII
;;    VVIIIIII
;;    XIIIIII
;;    VVVI
;;    XVI
;;
;; The last example being considered the most efficient, as it uses the 
;; least number of numerals.
;;
;; The 11K text file, 'data/roman.txt', contains one thousand numbers 
;; written in valid, but not necessarily minimal, Roman numerals; that is, 
;; they are arranged in descending units and obey the subtractive pair rule.
;;
;; Find the number of characters saved by writing each of these in their 
;; minimal form.
;;
;; Note: You can assume that all the Roman numerals in the file contain no
;; more than four consecutive identical units.
;; 

(use '[clojure.string :only (split-lines split)])

(defn calc-numeral [[cur nxt]]
  (let [numerals { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000 }
        sign (cond 
               (nil? (numerals nxt)) 1
               (< (numerals cur) (numerals nxt)) -1
               :else 1)]
    (* sign (numerals cur))))

(defn to-number [roman]
  (->> (concat (seq roman) [\0])
       (partition 2 1)
       (map calc-numeral)
       (reduce +)))

(defn get-best [n]
  (let [numerals [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
                  [100 "C"]  [90 "XC"]  [50 "L"]  [40 "XL"]
                  [10 "X"]   [9 "IX"]   [5 "V"]   [4 "IV"] 
                  [1, "I"]
                  [0, ""]]]
  (first (drop-while #(> (first %) n) numerals))))

(defn to-roman [n]
  (loop [res []
         n   n]
    (if (zero? n)
      (apply str res)
      (let [pair (get-best n)]
        (recur (concat res [(second pair)]) (- n (first pair)))))))

(defn calc-savings [roman]
  (let [n (to-number roman)
        r (to-roman n)]
    (- (count roman) (count r))))

(defn load-data [fname]
  (split-lines (slurp fname)))

(defn solve [fname]
  (->> (load-data fname)
       (map calc-savings)
       (reduce +)))

(time (solve "data/roman.txt"))
