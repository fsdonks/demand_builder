(ns demand_builder.jfreeoverride
  (:require [incanter core charts]
            [demand_builder [chart :as c]])
  (:import [org.jfree.chart.annotations CategoryAnnotation CategoryLineAnnotation TextAnnotation]
           [org.jfree.chart.plot Plot]
           [org.jfree.chart.axis CategoryAnchor]))

;;Temporary name space for adding/changing functionality of Jfreecharts, will eventually integrate into another ns



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

;;Source (java) for draw method from: https://github.com/jfree/jfreechart/blob/master/src/main/java/org/jfree/chart/annotations/CategoryLineAnnotation.java
;;Need to override this with proxy to change line annotation behavior for single categories
" public void draw(Graphics2D g2, CategoryPlot plot, Rectangle2D dataArea,
                     CategoryAxis domainAxis, ValueAxis rangeAxis) {
        CategoryDataset dataset = plot.getDataset();
        int catIndex1 = dataset.getColumnIndex(this.category1);
        int catIndex2 = dataset.getColumnIndex(this.category2);
        int catCount = dataset.getColumnCount();
        double lineX1 = 0.0f;
        double lineY1 = 0.0f;
        double lineX2 = 0.0f;
        double lineY2 = 0.0f;
        PlotOrientation orientation = plot.getOrientation();
        RectangleEdge domainEdge = Plot.resolveDomainAxisLocation(
            plot.getDomainAxisLocation(), orientation);
        RectangleEdge rangeEdge = Plot.resolveRangeAxisLocation(
            plot.getRangeAxisLocation(), orientation);

        if (orientation == PlotOrientation.HORIZONTAL) {
            lineY1 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex1, catCount, dataArea,
                domainEdge);
            lineX1 = rangeAxis.valueToJava2D(this.value1, dataArea, rangeEdge);
            lineY2 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex2, catCount, dataArea,
                domainEdge);
            lineX2 = rangeAxis.valueToJava2D(this.value2, dataArea, rangeEdge);
        }
        else if (orientation == PlotOrientation.VERTICAL) {
            lineX1 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex1, catCount, dataArea,
                domainEdge);
            lineY1 = rangeAxis.valueToJava2D(this.value1, dataArea, rangeEdge);
            lineX2 = domainAxis.getCategoryJava2DCoordinate(
                CategoryAnchor.MIDDLE, catIndex2, catCount, dataArea,
                domainEdge);
            lineY2 = rangeAxis.valueToJava2D(this.value2, dataArea, rangeEdge);
        }
        g2.setPaint(this.paint);
        g2.setStroke(this.stroke);
        g2.drawLine((int) lineX1, (int) lineY1, (int) lineX2, (int) lineY2);
    }
"
;;Will return a CategoryLineAnnotation proxy
;; the draw method will draw a line from the starting edge of category c1 to the ending edge of category c1 at value v1
;; a second category and value can be given as optional arguments (c1 always start edge, c2 always end edge) for the line
;; where c1,c2 are anchored to the domain axis and v1,v2 are anchored to the range axis.
;; paint is a java Color object which determines the color of the line
;; stroke is a java Stroke object which determines the stroke used to draw the line
(defn annotation [chart c1 v1 & {:keys [paint stroke c2 v2] :or {c2 c1 v2 v1 paint (java.awt.Color/black) stroke (java.awt.BasicStroke. 3 2 2)}}]
  (proxy [org.jfree.chart.annotations.CategoryLineAnnotation] [c1 v1 c2 v2 paint stroke]
    (draw [^java.awt.Graphics g2 cp dataArea da va] ;;Graphics2D g2 and Rectange2D dataArea automatically supplied by graphics interface
      (let [cp (.getCategoryPlot chart)
            ra dataArea
            da (.getDomainAxis cp) ;;domain axis
            va (.getRangeAxis cp) ;;value axis
            dset (.getDataset cp) ;;categorical data set
            catIndex (.getColumnIndex dset c1) ;;category index
            catCount (.getColumnCount dset) ;;numer of categories total
            domainEdge (org.jfree.chart.plot.Plot/resolveDomainAxisLocation (.getDomainAxisLocation cp) (.getOrientation cp))
            rangeEdge (org.jfree.chart.plot.Plot/resolveRangeAxisLocation (.getRangeAxisLocation cp) (.getOrientation cp))
            startAnchor (org.jfree.chart.axis.CategoryAnchor/START)
            endAnchor (org.jfree.chart.axis.CategoryAnchor/END)
            y1 (.valueToJava2D va v1 ra rangeEdge) ;;y1 = y2
            x1 (.getCategoryJava2DCoordinate da startAnchor catIndex catCount ra domainEdge)
            x2 (.getCategoryJava2DCoordinate da endAnchor catIndex catCount ra domainEdge)]
        (.setPaint g2 paint)
        (.setStroke g2 stroke)
        (.drawLine g2 x1 y1 x2 y1)))))        


;;Default JFree category line annotation behavior wrapper
;;Will add a black line at the point [category, value] on the chart 
;;Because category1 = category2, the line will be a single point, 
;;  CategoryPlots don't support just drawing a regular line of some specified length; (USE annotation function for single category line)
;;  can only draw a line between/across catagories because of the level of abstraction in jfree.chart.annotations 
(defn add-category-line [chart category value]
  (.addAnnotation (.getCategoryPlot chart)
    (org.jfree.chart.annotations.CategoryLineAnnotation. category value category value 
      (java.awt.Color/black) (java.awt.BasicStroke. 2 2 2 1 (float-array 3 1) 1))))

