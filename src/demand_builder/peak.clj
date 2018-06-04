(ns demand_builder.peak
  (:require [incanter core charts]
            [demand_builder [jfreeoverride :as jf]]
            [spork.util table [io :as io]])
  (:import [org.jfree.chart.annotations CategoryAnnotation CategoryLineAnnotation TextAnnotation]
           [org.jfree.chart.plot Plot]
           [org.jfree.chart.axis CategoryAnchor]))

;;Name space for building peak demand stacked bar charts. 
;; ***Input file will be different from the output from demand builder (demand output) ***
;;    The expected file format contains columns for SRC, AC, RC, NG, and peak
;;     where SRC identifies the unit,
;;           AC, RC, and NG are the supply of unit type on hand (supply side)
;;           and peak is the highest amount demanded at any given point (demand side)


;;Finds peak value and adds a line to chart
(defn add-peaks [chart map-list]
  (let [srcs (map #(:SRC %) map-list)]
    (doseq [m map-list 
            :let [peak (:peak m)
                  demand (apply + [(:AC m) (:RC m) (:NG m)])
                  color (if (< demand peak) (java.awt.Color/red) (java.awt.Color/black))]]
      (.addAnnotation (.getCategoryPlot chart) (jf/annotation chart (:SRC m) peak :paint color))))) 

;;Creates the stacked bar plot and adds the peak lines for each SRC
;;Returns the chart object
(defn build-peak-chart [map-list & {:keys [title]}]
  (let [chart (incanter.charts/stacked-bar-chart 
                (flatten (map #(repeat 3 (:SRC %)) map-list)) ;; categories
                (flatten (for [m map-list] [(:AC m) (:RC m) (:NG m)])) ;;supply (y-values)
                :group-by (flatten (repeat (count (set (map #(:SRC %) map-list))) ["AC" "RC" "NG"])) ;;group-by
                :legend true :x-label "SRC" :y-label "Number of units" :title title)
        font (java.awt.Font. "Tahoma" 0 10)
        srcs (map #(:SRC %) map-list)]
   (doseq [src srcs] (.setTickLabelFont (.getDomainAxis (.getCategoryPlot chart)) src font)) ;;Change font size of x-label
   (.setMaximumCategoryLabelLines (.getDomainAxis (.getCategoryPlot chart)) 11);;can change for readability of category labels
   (add-peaks chart map-list) ;;add peak demand line for each SRC
   chart))

;;Creates peak-demand chart from file
;;Returns the chart object
;; save keyword as true will save file in same directory as input file as filename-peaks.png
;; view keyword as true will display the plot
(defn file->peak-chart [filename & {:keys [save view] :or {save false view false}}]
  (let [chart (build-peak-chart (c/file->map-list filename) :title (str "Peak Demands: " (clojure.string/replace (io/fname filename) (str "." (io/fext filename)) ""))) 
        out (clojure.string/replace filename (str "." (io/fext filename)) "-peaks.png")]
    (when save (incanter.core/save chart out))
    (when view (incanter.core/view chart))
    chart))

