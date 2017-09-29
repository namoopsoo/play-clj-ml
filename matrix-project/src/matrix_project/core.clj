(ns matrix-project.core

;(ns mynamespace
  (:use [incanter.charts :only [xy-plot add-points scatter-plot add-lines]]
        [incanter.core :only [view]]
        [incanter.stats :only [linear-model]]
        )
   (:require
             [clojure.core.matrix :as mtrix]
             [clojure.core.matrix.operators :as M]
             [clatrix.core :as cl]
             
             [clojure.data.csv :as csv]
             [clojure.java.io :as io]) 
  )
             

(defn plot-points
  "plots sample points of a solution s"
  [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (view 
      (add-points
        (xy-plot X Y) (:observed s) (:observed-values s)))))
        

(def fname
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.simple.csv")

(def fname-tiny
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.simple.tiny.csv")


(defn pass
  [x]
  x)


(defn parse-row-as-float
  [table-row]
  (map #(Double/parseDouble %) table-row))


(defn parse-table-as-doubles
  [table]
  (map  #(parse-row-as-float %)  table))


(defn load-csv-data
  [fname]
  (let [
        table (with-open [reader (io/reader fname)]
                (->>
                  (csv/read-csv reader)
                  (mapv pass)))
        header-row (first table)
        columns (->>
                  (rest table)
                  (parse-table-as-doubles)
                  (mtrix/transpose))]
    {:header header-row
     :columns columns}
    ))

(defn plot-data-and-linear-model
  [X Y simple-model]
  (doall
    ; scatter-plot
    (def linear-simple-scatter (scatter-plot X Y))

    ; then add-lines from fitted model.
    (view (add-lines linear-simple-scatter X (:fitted simple-model)))
    ))


(def A (rest [["" "start_sublocality" "end_sublocality"] ["0" "2" "2"] ["1" "2" "2"] ["2" "2" "2"] ["3" "2" "2"] ["4" "2" "2"] ["5" "2" "2"] ["6" "2" "2"] ["7" "2" "2"] ["8" "2" "2"]]))

(def B (cl/matrix (parse-table-as-doubles A)))

(def C (mtrix/transpose B))

(def Y (cl/matrix (nth C 2)))
(def X (cl/matrix (nth C 1)))

(def samp-linear-model (linear-model Y X))


