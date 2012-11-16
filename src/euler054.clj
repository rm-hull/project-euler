
;; EULER #054
;; ==========
;; In the card game poker, a hand consists of five cards and are ranked,
;; from lowest to highest, in the following way:
;;
;;   * High Card: Highest value card.
;;   * One Pair: Two cards of the same value.
;;   * Two Pairs: Two different pairs.
;;   * Three of a Kind: Three cards of the same value.
;;   * Straight: All cards are consecutive values.
;;   * Flush: All cards of the same suit.
;;   * Full House: Three of a kind and a pair.
;;   * Four of a Kind: Four cards of the same value.
;;   * Straight Flush: All cards are consecutive values of same suit.
;;   * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;;
;; The cards are valued in the order:
;;
;;   2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;;
;; If two players have the same ranked hands then the rank made up of 
;; the highest value wins; for example, a pair of eights beats a pair
;; of fives (see example 1 below). But if two ranks tie, for example, 
;; both players have a pair of queens, then highest cards in each hand 
;; are compared (see example 4 below); if the highest cards tie then 
;; the next highest cards are compared, and so on.
;;
;; Consider the following five hands dealt to two players:
;;
;; Hand   Player 1          Player 2            Winner
;; ------------------------------------------------------
;; 1      5H 5C 6S 7S KD    2C 3S 8S 8D TD      Player 2
;;        Pair of Fives     Pair of Eights
;;
;; 2      5D 8C 9S JS AC    2C 5C 7D 8S QH      Player 1
;;        Highest card Ace  Highest card Queen
;;
;; 3      2D 9C AS AH AC    3D 6D 7D TD QD      Player 2
;;        Three Aces        Flush with Diamonds
;;
;; 4      4D 6S 9H QH QC    3D 6D 7H QD QS      Player 1
;;        Pair of Queens    Pair of Queens    
;;        Highest card Nine Highest card Seven
;;
;; 5      2H 2D 4C 4D 4S    3C 3D 3S 9S 9D      Player 1
;;        Full House        Full House
;;        With Three Fours  with Three Threes
;;
;; The file, 'data/poker.txt', contains one-thousand random hands dealt 
;; to two players. Each line of the file contains ten cards (separated 
;; by a single space): the first five are Player 1's cards and the last 
;; five are Player 2's cards. You can assume that all hands are valid 
;; (no invalid characters or repeated cards), each player's hand is in 
;; no specific order, and in each hand there is a clear winner.
;;
;; How many hands does Player 1 win?
;;

(ns euler054
  (:use [util.misc]))

(defn games [fname]
  (clojure.string/split-lines (slurp fname)))

(defn suit [card]
  (let [s (subs card 1)]
    ({"C" :clubs, "D" :diamonds, "H" :hearts, "S" :spades} s)))

(defn card [s]
  {:rank (subs s 0 1) :suit (suit s)})

(defn card-value [card]
  "Calculates the card value, T=10, J=11, etc"
  (let [r (card :rank)
        picture-values {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14}]  ;; Aces high
    (dec
      (if-let [x (picture-values r)]
        x
        (read-string r)))))

(defn parse-cards [data]
  "Split a game (10 cards) into two hands"
  (->> (clojure.string/split data #"\W")
       (map card)
       (partition 5)
       (map (partial sort-by card-value))))

(defn score [result]
  (cond 
    (nil? result) 0
    (empty? result) 0
    :else (card-value result)))

(defn runners-up [coll]
  (sort (map card-value coll)))

(defn highest-card [coll]
  (score (last coll)))

(defn lowest-card [coll]
  (score (first coll)))

(defn n-of-a-kind [n coll]
  (map #(hash-map :rank %) 
       (keys (filter #(= n (count (val %))) (group-by :rank coll)))))

(def pairs (partial n-of-a-kind 2))

(defn three-of-a-kind [coll] 
  (let [score (score (first (n-of-a-kind 3 coll)))]
    (if (pos? score)
      score
      0)))

(defn four-of-a-kind [coll] 
  (let [score (score (first (n-of-a-kind 4 coll)))]
    (if (pos? score)
      score
      0)))

(defn one-pair [coll]
  (let [p (pairs coll)]
    (if (= (count p) 1)
      (score (first p))
      0)))

(defn two-pairs [coll]
  (let [p (pairs coll)]
    (if (= (count p) 2) 
      (apply max (map score p))
      0)))

(defn poker-flush [coll]
  (if (= 5 (first (vals (frequencies (map :suit coll)))))
    (highest-card coll)
    0))

(defn full-house [coll]
  (let [pair (one-pair coll)
        tofk (three-of-a-kind coll)
        counts (sort (vals (frequencies (map :rank coll))))]
    (if (and (pos? pair) (pos? tofk) (= counts '(2 3)))
      tofk
      0)))

(defn straight [coll]
  (let [base (-> coll lowest-card dec)
        sum (reduce + (map #(- (card-value %) base) coll))]
    (if (= sum 15) 
      (highest-card coll) 
      0)))

(defn straight-flush [coll]
  (if (pos? (poker-flush coll)) 
    (straight coll) 
    0))

(defn royal-flush [coll]
  (if (= (straight-flush coll)) 13) 13 0)

(defn inflate-best [xs radix]
  (loop [xs xs
         tot 0N]
    (if (empty? xs)
      tot
      (recur 
        (rest xs)
        (+ (* tot radix) (bigint (if (pos? tot) 0 (first xs))))))))

(defn calculate-best-score [hand]
  (let [rules (list highest-card one-pair two-pairs 
                    three-of-a-kind straight poker-flush full-house 
                    four-of-a-kind straight-flush royal-flush)
        scores (->> rules (map #(% hand)) flatten reverse)]
    (inflate-best scores 16)))
    ;(to-number scores 16)))

(defn calculate-next-highest-score [hand]
  (to-number (reverse (runners-up hand)) 16))

(defn play-game [game]
  (let [hands       (parse-cards game)
        best-scores (map calculate-best-score hands)
        next-scores (map calculate-next-highest-score hands)]
    [ game (cond 
             (apply > best-scores) :player-one 
             (apply < best-scores) :player-two
             (apply > next-scores) :player-one 
             (apply < next-scores) :player-two 
             :else                 :tie)]))

(defn solve [fname]
  (->> (games fname)
       (map play-game)
       (map second)   
       frequencies
       ))

(def rules (list highest-card one-pair two-pairs 
                 three-of-a-kind straight poker-flush full-house 
                 four-of-a-kind straight-flush royal-flush))

(def hand1 (first (parse-cards "AH KC TS JS QD")))
(def hand2 (first (parse-cards "2C 2S 2D TC TD")))

(map #(% hand1) rules)

(map #(% hand2) rules)

(play-game "6D 7C 5D 5H 3S 5C JC 2H 5S 3D")

(pairs hand1)

(time (solve "data/poker-test.txt"))

(time (solve "data/poker.txt"))

;(doseq [result (solve "data/poker.txt")]
;  (prn result))





