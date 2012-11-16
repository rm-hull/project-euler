;; EULER #059
;; ==========
;; Each character on a computer is assigned a unique code and the preferred
;; standard is ASCII (American Standard Code for Information Interchange).
;; For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
;;
;; A modern encryption method is to take a text file, convert the bytes
;; to ASCII, then XOR each byte with a given value, taken from a secret 
;; key. The advantage with the XOR function is that using the same 
;; encryption key on the cipher text, restores the plain text; for 
;; example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
;;
;; For unbreakable encryption, the key is the same length as the plain
;; text message, and the key is made up of random bytes. The user would
;; keep the encrypted message and the encryption key in different 
;; locations, and without both "halves", it is impossible to decrypt 
;; the message.
;;
;; Unfortunately, this method is impractical for most users, so the 
;; modified method is to use a password as a key. If the password is 
;; shorter than the message, which is likely, the key is repeated 
;; cyclically throughout the message. The balance for this method is 
;; using a sufficiently long password key for security, but short enough 
;; to be memorable.
;;
;; Your task has been made easy, as the encryption key consists of three
;; lower case characters. Using 'data/cipher1.txt', a file containing the
;; encrypted ASCII codes, and the knowledge that the plain text must 
;; contain common English words, decrypt the message and find the sum of 
;; the ASCII values in the original text.
;;

(ns euler059
  (:use [util.combinatorics]))

(defn get-encrypted-bytes [fname]
  (map read-string (clojure.string/split (slurp fname) #",")))

(defn perms [coll]
  (letfn [(rotate [x] (concat (rest x) [(first x)]))]
    (list
      coll
      (rotate coll)
      (rotate (rotate coll)))))

(def pwd-combinations
  (mapcat perms (combinations (range 97 123) 3)))

(defn decrypt [data pwd]
  (apply str (map char (map bit-xor data (cycle pwd)))))

(defn all-solutions [fname search-word]
  (let [data (get-encrypted-bytes fname)]
    (filter #(.contains % search-word) 
            (pmap #(decrypt data %) pwd-combinations))))

(defn sum-str [s] 
  (reduce + (map int (seq s))))

(defn solve [fname search-word]
  (first (map #(array-map :sum (sum-str %) :text %) (all-solutions fname search-word))))

(time (solve "data/cipher1.txt" "the "))
