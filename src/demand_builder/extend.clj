(ns demand_builder.extend
  (:require [demand_builder [chart :as c] [jfreeoverride :as jf] [formatter :as f]])) 

;;Given a file with StartDay and Duration, will return the start and end days for each group, 
;;where groupby is the name (string) of another field in the file 
;;Example: (get-times demandfile Vignette) 
(defn get-times [filename groupby]
  (let [data (jf/group-by-key (c/file->map-list filename) groupby)]
    (for [k (keys data) :let [g (sort-by :StartDay (get data k))]] 
     [k (first (map :StartDay g)) (+ (:StartDay (last g)) (:Duration (last g)))])))
  
;;Will adjust values in map-list which groupkey match key by delta.
;;Map-list is a list of maps (records)
;;Valkey is a keyword of the value in file (such as :Quantity, :Strength, :Duration, ect) **has to be numerical
;;Groupkey is a keyword of the column in the file that the data is to be grouped by
;;Key is the specific group value
;;Delta is the amount of val (from val key) to update 
(defn adjust-val [map-list valkey groupkey key delta]
  (let [no-change (remove #(= key (groupkey %)) map-list)
        update (filter #(= key (groupkey %)) map-list)]
    (flatten (conj no-change (map #(assoc % valkey (+ delta (valkey %))) update)))))

;;Make all updates in list of updates to map-list, 
;;Where an update is a map with keys :valkey (keyword) :groupkey (keyword) :key (value of specific group) delta (number)
;;example: [{:valkey :StartDay :groupkey :DemandGroup :key "SE-99" :delta 100}]
;;**Updates will be preformed in-order given
(defn multi-adjust [map-list updates]
  (if-not (zero? (count updates)) 
    (let [args (first updates)]
      (multi-adjust (adjust-val map-list (:valkey args) (:groupkey args) (:key args) (:delta args)) (drop 1 updates)))
    map-list))

;;Updates the values from input file with updates (see multi-adjust) and saves to outfile. 
(defn updates->file [infile outfile updates & {:keys [outorder] :or {outorder nil}}]
  (spork.util.stream/records->file (multi-adjust (c/file->map-list infile) updates) outfile :field-order outorder))


(def f "C:\\Users\\michael.m.pavlak.civ\\Documents\\demand_builder\\test\\resources\\complexest\\Input\\Input_DEMAND.txt")
(defn rf [] (require 'demand_builder.extend :reload))
