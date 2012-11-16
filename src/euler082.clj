;; EULER #082
;; ==========
;; NOTE: This problem is a more challenging version of Problem 81.
;;
;; The minimal path sum in the 5 by 5 matrix below, by starting in any cell
;; in the left column and finishing in any cell in the right column, and 
;; only moving up, down, and right, is indicated in red and bold; the sum
;; is equal to 994.
;;
;;    131  673  234  103   18
;;    201   96  342  965  150
;;    630  803  746  422  111
;;    537  699  497  121  956
;;    805  732  524   37  331
;;
;; Find the minimal path sum, in 'data/matrix.txt',  a 31K text file 
;; containing a 80 by 80 matrix, from the left column to the right column.
;;

(use '[clojure.string :only (split-lines split)])

(defn load-data [fname]
  (letfn [(parse-nums [s] (vec (map #(Long/parseLong %) (split s #"\W"))))]
    (vec (map parse-nums (split-lines (slurp fname))))))

(defn estimate-cost [step-cost-est [goal-x goal-y] [curr-x curr-y]]
  (let [dx (- goal-x curr-x)
        dy (- goal-y curr-y)]
  (* step-cost-est
     (+ (* dx dx) (* dy dy)))))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est goal-xy curr-xy]
  (+ newcost
     (estimate-cost step-cost-est goal-xy curr-xy)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this] (if (> (f min) (f this)) this min))
            coll)))

(defn neighbors
  ;([size xy] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size xy))
  ([size xy] (neighbors [[1 0] [0 1]] size xy))
  ([deltas size xy]
     (filter (fn [new-xy]
               (every? #(< -1 % size) new-xy))
             (map #(vec (map + xy %)) deltas))))
   
(defn a*-search [start-xy goal-xy step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (repeat size (vec (repeat size nil))))
           work-todo (sorted-set [0 start-xy])]
      (if (empty? work-todo)
        (let [[x y] goal-xy]
          [(nth (nth routes y) x) :steps steps])
        (let [[_ xy :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-xys (neighbors size xy)
              cheapest-nbr (min-by :cost 
                                   (keep #(get-in routes %)
                                         nbr-xys))
              newcost (path-cost (get-in cell-costs xy)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes xy))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps) 
                   (assoc-in routes xy
                             {:cost newcost 
                              :xys (conj (:xys cheapest-nbr []) xy)})
                   (into rest-work-todo
                         (map 
                           (fn [w]
                             (let [[x y] w]
                               [(total-cost newcost step-est goal-xy [x y]) w]))
                           nbr-xys)))))))))


(def world 
  (load-data "data/80-matrix.txt"))


(a*-search [0 0] [79 79] 99999 world )
