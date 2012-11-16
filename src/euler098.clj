;; EULER #098
;; ==========
;; By replacing each of the letters in the word CARE with 1, 2, 9, and 6 
;; respectively, we form a square number: 1296 = 36^2. What is remarkable
;; is that, by using the same digital substitutions, the anagram, RACE, 
;; also forms a square number: 9216 = 96^2. We shall call CARE (and RACE)
;; a square anagram word pair and specify further that leading zeroes are 
;; not permitted, neither may a different letter have the same digital value 
;; as another letter.
;;
;; Using 'data/words.txt', a 16K text file containing nearly two-thousand 
;; common English words, find all the square anagram word pairs (a 
;; palindromic word is NOT considered to be an anagram of itself).
;;
;; What is the largest square number formed by any member of such a pair?
;;
;; NOTE: All anagrams formed must be contained in the given text file.
;;

(ns euler098
  (:use [util.misc]
        [util.palindromes]))

(defn strip-quotes [s]
  (apply str (filter #(not= % \") s)))

(defn get-data [fname]
  (map 
    strip-quotes
    (clojure.string/split 
      (slurp fname) #",")))

(defn anagrams [fname]
  (->> (get-data fname)
       (map #(hash-map (sort %) [%]))
       (apply merge-with concat)
       (filter #(> (count (val %)) 1))
       (map val)))

(defn squares-range [digits]
  (let [lower (iexpt 10 (dec digits))
        upper (* lower 10)]
    (->> squares
         (drop-while #(< % lower))
         (take-while #(< % upper)))))

(defn create-mapping [word n]
  (zipmap (seq word) (digits n)))

(defn apply-mapping [word mapping]
  (->> word
       (replace mapping)
       (apply str)
       (Integer/parseInt)))

(defn square-anagrams [[a b _]]
  (let [dig (count a)]
    (for [sq (squares-range dig)
          :let [mapping (create-mapping a sq)
                result  (apply-mapping b mapping)]
          :when (and
                  (is-square? result)
                  (= (count (str sq)) (count (str result)))
                  (= dig (count (distinct (vals mapping)))))]
      result)))

(defn solve [fname]
  (->> (anagrams fname)
       (map square-anagrams)
       flatten
       (reduce max)))

(time (solve "data/words.txt"))
