(ns demand_builder.peak
  (:require [incanter core charts]
            [demand_builder [chart :as c]])
  (:import org.jfree.chart.annotations.CategoryLineAnnotation))

;;Name space for building peak demand stacked bar charts. 
;; ***Input file will be different from the output from demand builder (demand output) ***
;;    The expected file format contains columns for SRC, AC, RC, NG, and peak
;;     where SRC identifies the unit,
;;           AC, RC, and NG are the supply of unit type on hand (supply side)
;;           and peak is the highest amount demanded at any given point (demand side)

;;Will add a black line at the point [category, value] on the chart 
;;Because category1 = category2, the line will be a single point, 
;;  CategoryPlots don't support just drawing a regular line of some specified length;
;;  can only draw a line between/across catagories because of the level of abstraction in jfree.chart.annotations 
(defn add-category-line [chart category value]
  (.addAnnotation (.getCategoryPlot chart)
    (org.jfree.chart.annotations.CategoryLineAnnotation. category value category value (java.awt.Color/black) (java.awt.BasicStroke. 5 2 2))))

(defn add-peaks [chart map-list]
  (let [srcs (map #(:SRC %) map-list)]
    (doseq [i (range (count srcs)) 
            :let [pre (if (> 0 (dec i)) (nth srcs i) (nth srcs (dec i)))
                  post (if (<= (count srcs) (inc i)) (nth srcs i) (nth srcs (inc i)))
                  peak (c/read-num (:peak (nth map-list i)))]]
      (.addAnnotation (.getCategoryPlot chart)
        (org.jfree.chart.annotations.CategoryLineAnnotation. pre peak post peak (java.awt.Color/black) (java.awt.BasicStroke. 1.75 2 2))))))

;;Creates the stacked bar plot and adds the peak lines for each SRC
;;Returns the chart object
(defn build-peak-chart [map-list]
  (let [chart (incanter.charts/stacked-bar-chart 
                (flatten (map #(c/rep (:SRC %) 3) map-list)) ;; categories
                (flatten (for [m map-list] [(c/read-num (:AC m)) (c/read-num (:RC m)) (c/read-num (:NG m))])) ;;supply (y-values)
                :group-by (flatten (c/rep ["AC" "RC" "NG"] (count (set (map #(:SRC %) map-list))))) ;;group-by
                :legend true :x-label "SRC" :y-label "Number of units")
        font (java.awt.Font. "Tahoma" 0 10)
        srcs (map #(:SRC %) map-list)]
   (doseq [src srcs] (.setTickLabelFont (.getDomainAxis (.getCategoryPlot chart)) src font)) ;;Change font size of x-label
   (.setMaximumCategoryLabelLines (.getDomainAxis (.getCategoryPlot chart)) 11);;can change for readability of category labels
   (add-peaks chart map-list) ;;add peak demand line (dot) for each SRC
   chart))

;;Creates peak-demand chart from file
;;Returns the chart object
;; save keyword as true will save file in same directory as input file as filename-peaks.png
;; view keyword as true will display the plot
(defn file->peak-chart [filename & {:keys [save view] :or {save false view false}}]
  (let [chart (build-peak-chart (c/file->map-list filename)) 
        out (str (apply str (take (- (count filename) 4) filename)) "-peaks.png")]
    (when save
      (incanter.core/save chart out))
    (when view 
      (incanter.core/view chart))
    chart))

