;; EULER #022
;; ==========
;; Using 'data/names.txt', a 46K text file containing over five-thousand
;; first names, begin by sorting it into alphabetical order. Then working
;; out the alphabetical value for each name, multiply this value by its 
;; alphabetical position in the list to obtain a name score.
;;
;; For example, when the list is sorted into alphabetical order, COLIN, 
;; which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. 
;; So, COLIN would obtain a score of 938 x 53 = 49714.
;;
;; What is the total of all the name scores in the file?
;;

(ns euler022
  (:use [clojure.string]
        [util.misc]))

(defn get-data [fname]
  (sort (split (slurp fname) #",")))

(defn name-score [idx nm]
  (let [v (reduce + (map #(- (int %) 64) (filter #(not= % \") nm)))]
    (* v idx)))

(defn solve [fname]
  (reduce + 
    (map name-score
         integers
         (get-data fname))))

(time (solve "data/names.txt"))
