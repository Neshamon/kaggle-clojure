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

(def bin-arr '())

(defn bincounts [a]
  (keep #(if (pos-int? %) %) (vals (frequencies a))))

(int? -99)

(bincounts [1 3 3 3 3 5 6 1 9])

(println bin-arr)

(defn log-base-n [base n]
  (/ (Math/log n) (Math/log base)))

(defn entropy [a]
  (let [counts (bincounts a)]
    (let [percentage (map #(/ % (count a)) counts)]
      (let [chaos 0]
           (map #(if (< 0 %)
                   (recur (+ chaos (* % (log-base-n 2 %))))) percentage)))))

(entropy [0 0 0 0 0 0 0 0 0 1 1 1])
