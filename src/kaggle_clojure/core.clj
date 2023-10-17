(ns kaggle-clojure.core
  (:require [tech.v3.dataset :as ds]
            [tech.v3.dataset.io.datetime :as dt]
            [tech.v3.dataset.Modelling :as ml]
            [scicloj.ml.core :as sci]))

(def house-path "resources/melb_data.csv")

(defn -main
  "I don't do a whole lot."
  []
  (println "Working!"))

(def melb-dataset
  (ds/->dataset house-path))

(def iowa-homes
  (ds/->dataset "resources/train.csv"))

(defn show-melb []
  (println melb-dataset))

;; Iowa data set (tran.csv) exercises begin here

; What is the average lot size (rounded to nearest integer)?

(defn show-iowa []
  (println iowa-homes))

(def lot-area
  (ds/select-columns iowa-homes ["LotArea"]))

(defn averager [column]
  (int (/ (reduce + (flatten (ds/rowvecs column))) (ds/row-count column))))

(averager lot-area) ;;-> 10516. Off by 1?

;; As of today, how old is the newest home? (current year - the date in which it was built)

(def year-built
  (ds/select-columns iowa-homes ["YearBuilt"]))

(def max-year
  (ds/sort-by-column year-built "YearBuilt" >))

(defn measure-age [column]
  (- 2023 (first (first (ds/rowvecs column)))))

(measure-age max-year) ;; => 13

;; First ML Tutorial

(def prediction-target
  (ds/select-columns melb-dataset ["Price"]))

(def melb-features
  (ds/select-columns melb-dataset ["Rooms" "Bathroom" "Landsize" "Lattitude" "Longtitude"]))

;; Couldn't find any decision tree models available in clojure,
;; so I'm building one from scratch

(defn bincounts
  "This is a shallow reproduction of NumPy's bincount.
  This function takes a vector, maps over all occurences
  of a element and sets them up in a key-value set.

  Then the values of frequencies is then retrieved by vals.
  This separates each element to their own data bin yet still
  isolates the duplication count.

  After that I iterate over the new sequence with keep with
  a positive integer predicate. This makes it so only positive
  integers are returned by this function"
  [a]
  (keep #(if (pos-int? %) %) (vals (frequencies a))))

(bincounts [1 3 3 3 3 5 6 1 9]) ;; => {1 2, 3 4, 5 1, 6 1, 9 1} => (2 4 1 1 1)

(defn log-base-n
  "This function allows you to use any base you want
  with the log function."
  [base n]
  (/ (Math/log n) (Math/log base)))

(defn entropy
  "This is a implementation of the entropy formula.

  It measures the purity of a split at the node level
  ranging from 0 (pure) to 1 (impure) in decision trees."
  [a]
  (let [counts (bincounts a)]
    (let [percentage (map float (map #(/ % (count a)) counts))]
      (let [chaos (map #(if (< 0.0 %)
                          (* % (log-base-n 2 %))) percentage)]
        (* (reduce + chaos) -1))))) ;; This only took me 5 hours

(entropy [0 0 0 0 0 0 0 1 1 1]) ;; => 0.88129...

(defn information-gain
  "This represents an average of all entropy values based
  on a specific split. The higher the info gain value,
  the better the decision split is."
  [parent-node left-child right-child]
  (let [num-left (float (/ (count left-child) (count parent-node)))]
    (let [num-right (float (/ (count right-child) (count parent-node)))]
      (let [gain (- (entropy parent-node) (+ (* num-left (entropy left-child))
                                             (* num-right (entropy right-child))))]
        gain))))

(information-gain [ 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 ]
                  [ 0 0 0 0 0 0 0 0 0 0 1 1 ]
                  [ 0 0 0 0 1 1 1 1 ]) ;; => 0.180937...

(defrecord Node [feature
                 threshold
                 data_left
                 data_right
                 gain value])

(defprotocol DecisionTreeRegression
  (init [self min-sample-split, max-depth]
    ())
  (_entropy [s]
    (entropy s))
  (_information-gain [parent-node left-child right-child]
    (information-gain parent-node left-child right-child))
  (best-split [features y]
    (let [best_split {}]
      (let [best-info-gain -1]
        (let [n-cols (first (ds/shape melb-features))
              n-rows (last (ds/shape melb-features))]
          (map )))))
  (build [x y depth]
    ())
  (fit [x y]
    ())
  (_predict [x]
    ())
  (predict [x]
    ()))

(defrecord DecisionTree
    )
