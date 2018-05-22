(ns demand_builder.chart
   (:require [demand_builder [core :refer [read-num]]]
             [clojure.java [io :as io]]
             [spork.util [io :refer [list-files fpath fname fext write! writeln!]]]))

;;Name space for formatting charts using data from demand builder output

(defn read-formatted-demand [filename]
  (with-open [r (clojure.java.io/reader filename)] 
       (let [firstline (clojure.string/split (first (line-seq r)) #"\t")
             header (zipmap (range (count firstline)) firstline)
             lines (map #(clojure.string/split % #"\t") (line-seq r)) ;;header alread read, no need to drop
             data (for [line lines]
                    (zipmap (map keyword firstline) line))]
         ;(println firstline)
         ;(doseq [d data] (identity d))
         (group-by #(get % :Vignette) data))))
         
(defn vignette-start [vdata]
  (apply min (map #(read-num (:StartDay %)) vdata)))

(defn vignette-end [vdata]
  (apply max (map #(+ (read-num (:Duration %)) (read-num (:StartDay %))) vdata)))

(defn in-period [vdata start end]
  (filter #(and (>  start (read-num (:StartDay %))) 
                (< end (+ (read-num (:Duration %)) (read-num (:StartDay %)))))
           vdata))

;;; ***NO QUANTITY VARIABLE IN DEMAND FILE, USING MANUALLY ADDED PEOPLE FOR TESTING ***
(defn total-at-time [vdata time]
  (apply + (map #(read-num (:People %)) (in-period vdata time (inc time)))))

(defn coords [vdata]
  (let [times (range (vignette-start vdata) (vignette-end vdata))
        quantity (map #(total-at-time vdata %) times)]
    {:times times :quantities quantity}))

;; *** ADDED PEOPLE FIELD IN TEST DATA, REAL DEMAND FILE HAS NO QUANTITY VALUES, WILL NEED TO BE LOOKED UP ***
;;Returns a list of maps with the Vignette, sequence of time periods it is active, and aggregate quantity at the corresponding time
(defn get-plot-data [data]
  (for [k (keys data)]
    (conj {:Vignette k} (coords (get data k)))))

