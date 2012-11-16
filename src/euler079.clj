;; EULER #079
;; ==========
;; A common security method used for online banking is to ask the user for
;; three random characters from a passcode. For example, if the passcode 
;; was 531278, they may ask for the 2nd, 3rd, and 5th characters; the 
;; expected reply would be: 317.
;;
;; The text file, 'data/keylog.txt', contains fifty successful login 
;; attempts.
;;
;; Given that the three characters are always asked for in order, analyse
;; the file so as to determine the shortest possible secret passcode of 
;; unknown length.
;;

(use '[clojure.string :only (split-lines split)])
(use 'clojure.set)

(defn char-num [c]
  (- (int c) 48))

(defn get-data [fname]
  (map #(map char-num (seq %)) 
           (split-lines (slurp fname))))

(defn possible-positions [data filter-fn]
  (let [data (map #(filter filter-fn %) data)
        third (fn [x] (if (= 3 (count x)) (nth x 2)))]
    (hash-map 
      :start  (set (map first data))
      :middle (set (map second data))
      :end    (set (map third data)))))

(defn next-digit [data not-in]
  (let [filter-fn (fn [x] (not-any? #(= % x) not-in))
        pos (possible-positions data filter-fn)]
    (difference (:start pos) (:middle pos))))

(defn solve [fname]
  (let [data (get-data fname)]
    (loop [result nil]
      (let [next-num (next-digit data result)]
        (if (empty? next-num)
          result
          (recur (concat result next-num)))))))

(time (solve "data/keylog.txt"))
