(ns demand_builder.peak
  (:require [incanter core charts]
            [demand_builder [chart :as c]]
            [spork.util [table :as t]])
  (:import  [org.jfree.chart.annotations CategoryAnnotation CategoryLineAnnotation TextAnnotation]))
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
    (org.jfree.chart.annotations.CategoryLineAnnotation. category value category value 
      (java.awt.Color/black) (java.awt.BasicStroke. 12 2 2 1 (float-array 3 1) 1))))

;;Need to find a way to change how CategoryLineAnnotations works.
;;When given a two categories, draws a line from the middle of the first category to the middle of the second.
;;Have to change this to accept a single category and draw the line from the the category start to the category end.
;;These values can be found by calling (.getCategoryStart/End categoryPlot i i-count (java.awt.Rectange. 500 400) (.getDomainAxisEdge categoryPlot))
;; which returns a double. 

;;Still have to find a way to change the end/start location on the line annotations, don't know which draw method has to be overriden
;; sources: http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/annotations/CategoryLineAnnotation.html
;;          http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/plot/
;;Still need to find out what method the draw method of CategoryAnnotation uses internally. 
;; I assume internally it calls find middle on category c1 and category c2 then calls someother draw inherited draw function.
(defn annotation [chart c1 v1]
  (let [cp (.getCategoryPlot chart)
        paint (java.awt.Color/black)
        stroke (java.awt.BasicStroke. 2 2 2)
        ra (java.awt.Rectangle. 500 400)
        da (.getDomainAxis cp)
        va (.getRangeAxis cp)
        c2 c1 v2 v1]
    (proxy [org.jfree.chart.annotations.CategoryLineAnnotation] [c1 v1 c2 v2 paint stroke]
      (draw [g2 cp ra da va]
        ;(println "DRAWING") ;;need to get it to set as bar edge start and bar edge end
        (proxy-super draw g2 cp ra da va)))))

(defn add-peaks [chart map-list]
  (let [srcs (flatten [[""] (map #(:SRC %) map-list) [" "]])]
    (doseq [i (range 1 (dec (count srcs)))
            :let [peak (if (<= i (count map-list)) (:peak (nth map-list (dec i))) 0)
                  pre (if (> 0 (dec i)) (nth srcs i) (nth srcs (dec i)))
                  post (if (<= (count srcs) (inc i)) (nth srcs i) (nth srcs (inc i)))
                  m (nth map-list (dec i))
                  demand (apply + [(:AC m) (:RC m) (:NG m)])
                  color (if (< demand peak) (java.awt.Color. 205 0 0) (java.awt.Color. 0 205 0))]]
      (.addAnnotation (.getCategoryPlot chart) (annotation chart (nth srcs i) peak))))) 

      
      ;;(.addAnnotation (.getCategoryPlot chart)
       ;; (org.jfree.chart.annotations.CategoryLineAnnotation. pre peak post peak color 
       ;;   (java.awt.BasicStroke. 2 2 2 10 (float-array [1 4]) 0))))
;(comment (float-array [0.00001 20 10]))


;;Creates the stacked bar plot and adds the peak lines for each SRC
;;Returns the chart object
(defn build-peak-chart [map-list & {:keys [title]}]
  (let [chart (incanter.charts/stacked-bar-chart 
                (flatten [["" "" ""] (map #(c/rep (:SRC %) 3) map-list) [" " " " " "]]) ;; categories
                (flatten [[0 0 0] (for [m map-list] [(:AC m) (:RC m) (:NG m)]) [0 0 0]]) ;;supply (y-values)
                :group-by (flatten (c/rep ["AC" "RC" "NG"] (+ 2 (count (set (map #(:SRC %) map-list)))))) ;;group-by
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
  (let [chart (build-peak-chart (c/file->map-list filename) :title (str "Peak Demands: " (last (clojure.string/split filename #"[\\|/]")))) 
        out (str (apply str (take (- (count filename) 4) filename)) "-peaks.png")]
    (when save (incanter.core/save chart out))
    (when view (incanter.core/view chart))
    chart))

(comment
  (defn temp [c1 v1 c2 v2 paint stroke]
    (proxy [org.jfree.chart.annotations.CategoryLineAnnotation] [c1 v1 c2 v2 paint stroke] 
      (draw [g2 plot dataArea domainAxis rangeAxis]
        (proxy-super draw g2 plot dataArea domainAxis rangeAxis)))))
;;draw(Graphics2D g2, CategoryPlot plot, Rectangle2D dataArea, ^CategoryAxis domainAxis, ^ValueAxis rangeAxis)



;(into [] (t/tabdelimited->records filename))
;tabdelimited->records
;"C:\\Users\\michael.m.pavlak.civ\\Documents\\test.txt"
