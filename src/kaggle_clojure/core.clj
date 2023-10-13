(ns kaggle-clojure.core
  (:require [tech.v3.dataset :as ds]))

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

(defn show-iowa []
  (println iowa-homes))

(def lot-area
  (ds/select-columns iowa-homes ["LotArea"]))

(defn averager [a]
  (int (/ (reduce + (flatten (ds/rowvecs a))) (ds/row-count a))))

(averager lot-area)
