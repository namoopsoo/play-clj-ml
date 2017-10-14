(ns matrix-project.core

  (:use [incanter.charts :only [xy-plot add-points scatter-plot add-lines]]

        ;[incanter.core :only [view]]
        [incanter.stats :only [linear-model]]

        [lambda-ml.random-forest :only [
                                        make-random-forest-classifier
                                        random-forest-fit
                                        random-forest-predict

                                        ]]
        [lambda-ml.decision-tree :only [
                                        make-classification-tree
                                        decision-tree-fit
                                        decision-tree-predict
                                        gini-impurity
                                        ]]
        )
   (:require
     [clojure.core.matrix :as mtrix]
     [clojure.core.matrix.operators :as M]
     [clatrix.core :as cl]
     
     [clojure.data.csv :as csv]
     [clojure.java.io :as io]

     [incanter.core :as incore]
     [incanter.io :as incio]

     ) 
  )

(defn pass
  [x]
  x)
             
(defn plot-points
  "plots sample points of a solution s"
  [s]
  (let [X (concat (:hidden s) (:observed s))
        Y (concat (:hidden-values s) (:observed-values s))]
    (incore/view 
      (add-points
        (xy-plot X Y) (:observed s) (:observed-values s)))))
        

(def fname
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.simple.csv")

(def fname-tiny
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.simple.tiny.csv")

(def fname-medium
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.medium-simple.csv")


(def fname-medium-train
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.medium-simple-train.csv")
(def fname-medium-holdout
  "/Users/michal/LeDropbox/Dropbox/Code/repo/learn-citibike/datas/201510-citibike-tripdata.medium-simple-holdout.csv")


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


(defn csv-data->maps
  "Make sequence of maps from input csv data.
  
  Lifted from examples in https://github.com/clojure/data.csv"
  [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
	  (rest csv-data)))


(defn load-csv-data-to-maps
  [fname]
  (with-open [reader (io/reader fname)]
                (->>
                  (csv/read-csv reader)
                  (mapv pass)
                  (csv-data->maps)
                  )))


(defn make-super-simple-linear-model
  "Bake a single input variable linear regression.
  
  Let input be a map with :header and :columns"
  [data-table]
  (let [
        Y (nth (:columns data-table) 2)
        X (nth (:columns data-table) 1)
        simple-linear-model (linear-model Y X)
        ]
    simple-linear-model))


(defn simple-predict
  [simple-model X]
  (let [
        coefs (simple-model :coefs) 
        beta_coef (first coefs)
        error_coef (last coefs)
        ]
    (->>
      (map #(* % beta_coef) X)
      (map #(+ % error_coef))
      )))


(defn plot-data-and-linear-model
  [X Y simple-model n]
  (do
    ; scatter-plot
    (def linear-simple-scatter (xy-plot (take n X) (take n Y)))

    ; then add-lines from fitted model.
    (incore/view (add-lines linear-simple-scatter (take n X) (take n (:fitted simple-model))))
    ))

(comment
  "Ah, this is the incanter way of reading a csv..."

  (def data-file "data/all_160_in_51.P35.csv")
  (def va-data (incio/read-dataset data-file :header true))
  )


(defn take-subset-dataset
  [dataset indices]
  (let [indices-set (set indices)]
    (incore/$where  
     {:index {:$fn (fn [x]
                     (= (conj indices-set x) indices-set)
                     )}} 
     dataset)) 
  )


(def data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]])
(def fit
  (let [n 1001
        min-split 2
        min-leaf 1
        max-features 2]
    (-> (make-random-forest-classifier n min-split min-leaf max-features)
        (random-forest-fit data))))


(defn do-forest-train-classifier
  [training-matrix]
  (let [n 10  ;this didnt work though...
        min-split 2
        min-leaf 1
        max-features 2]
    (-> (make-random-forest-classifier n min-split min-leaf max-features)
        (random-forest-fit training-matrix)))
  )


(defn do-tree-train-classifier
  [training-matrix]
  (let [min-split 2
        min-leaf 1
        max-features 2]
    (-> (make-classification-tree gini-impurity min-split min-leaf max-features)
        (decision-tree-fit training-matrix)))
  )


(defn train-holdout-split
  [X]
  
  )



(comment
  (def A (rest [["" "start_sublocality" "end_sublocality"] ["0" "2" "2"] ["1" "2" "2"] ["2" "2" "2"] ["3" "2" "2"] ["4" "2" "2"] ["5" "2" "2"] ["6" "2" "2"] ["7" "2" "2"] ["8" "2" "2"]]))

  (def B (cl/matrix (parse-table-as-doubles A)))

  (def C (mtrix/transpose B))

  (def Y (cl/matrix (nth C 2)))
  (def X (cl/matrix (nth C 1)))

  (def samp-linear-model (linear-model Y X))
)

(defn -main
  "I don't do a whole lot...yet."
  [& args]
   (println "Hello, World!"))

