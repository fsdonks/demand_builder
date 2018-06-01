(ns demand_builder.chart
  (:require [incanter core charts]
            [spork.util [table :as t]]
            [demand_builder [jfreeoverride :as jf]]))


;;Name space for Stacked chart generation using temporal data
;;Genic functionality for re-usability

;;Specific function to create sand charts for demand from file 
;;  ***uses specific file format, works as of May 2018***
;;     changes to demand file will require (ONLY) demand-file->sand-charts to be updated

;;Function to parse strings with exta syntax and convert to int without throwing error
(defn read-num [string]
  (try
    (if string
      (let [num (fn [string] (if (= string (re-matches #"[\d.,\"]*" string))
                               (apply str (map #(re-matches #"[\d.]*" %) (map str string)))
                               string))
            n (clojure.string/split (num string) #"[.]")  t (take 2 n) b (drop 2 n)
            d (read-string (str (first t) "." (second t) (apply str b)))]
        (if (zero? (- d (int d))) (int d) d))
      0)
    (catch Exception e string)))

;;Converts flat datafile into list of maps using keys from first line (header) of file
(defn file->map-list [filename & {:keys [del] :or {del "\t"}}]
 (with-open [r (clojure.java.io/reader filename)]
   (let [firstline (clojure.string/split (first (line-seq r)) (re-pattern del))
         header (zipmap (range (count firstline)) firstline)
         lines (pmap #(clojure.string/split % (re-pattern del)) (line-seq r))
         data (for [line lines] (zipmap (map keyword firstline) line))]
     (into [] data))))



;;Creates a stacked-xy-chart from data in supplied file
;; -filename is the filepath
;; -groupby-key is the column name to group by (String) *has to match what is in the file header
;; -startfn is the function that is used to calculate the start day in the data (expects a number)
;; -endfn is the function that is used to calculate the end data in the data (expects a number)
;; -yfn is the function that is used to get the y-value for the chart
;;   by having these as functions, can use for more types of data sets
;;   and allows for more complex combinations of the data
;;   example: yfn = #(min (- (* (:a %) (:b %)) (:c %)) (:d %)) *where a b c and d are columns in data
;; -title, x-label, and y-label are optional agruments for formatting the chart
;; -save will save the chart in the same filepath as filename-sand.png
;; -view will display the chart on the screen
;; Returns the jfree char
(defn file->stacked-xy-chart [filename groupby-key startfn endfn yfn
                              & {:keys [title x-label y-label] :or {title "" x-label "Time" y-label "Quantity"}}]
  (let [ds (jf/->ds (file->map-list filename) groupby-key yfn startfn endfn)
        chart (jf/stacked-xy-chart ds :title title :x-label x-label :y-label y-label)]
    chart))

;;Function to build sand chart from formatted demand file
;;Save will save the chat as a png with the filename filename-SandChart.png in the same directory as the original file
;;View will create a JFrame and set it as visible
;;Cont will make the changes between time periods continous, otherwise the changes will be discrete
;;Returns JFreeChart object
(defn file->sand-charts [filename key startfn endfn yfn 
                         & {:keys [save view title x-label y-label] 
                            :or {save false view true title "Sand Chart" x-label "Time" y-label "Quantity"}}]
  (let [prefix (first (clojure.string/split (last (clojure.string/split filename #"[/|\\]")) #"[.]"))
        chart (file->stacked-xy-chart filename key startfn endfn yfn 
                :save save :view view :title (str prefix " - Sand Chart") :x-label x-label :y-label y-label)] 
    (when save (incanter.core/save chart (str (apply str (take (- (count filename) 4) filename)) "-SandChart.png")))
    (when view (incanter.core/view chart))
    chart))

;;; =============== FUNCTIONS SPECIFIC FOR DEMAND FILE SAND CHARTS ================
;;Function for Sand Charts from formatted demand file
;;If demand file does not contain strength (people) per src, a supply file can be supplied to pull the data from
(defn demand-file->sand-charts [demandfile & {:keys [save view cont supplyfile] :or {save false view true cont true}}]
  (let [startfn #(read-num (:StartDay %))
        endfn #(+ (read-num (:StartDay %)) (read-num (:Duration %)))]
    (if (not= nil supplyfile)
      (let [supply (set (for [m (file->map-list supplyfile)] [(:SRC m) (:Strength m)]))
            strmap (zipmap (map first supply) (map second supply))]
        (file->sand-charts demandfile "DemandGroup" startfn endfn #(* (read-num (:Quantity %)) (max 1 (read-num (get strmap (:SRC %))))) :save true))
      (file->sand-charts demandfile "DemandGroup" startfn endfn #(* (read-num (:Quantity %)) (max 1 (read-num (:People %)))) :save true))))
;;; ===============================================================================




;;; ========== OLD, DON'T USE =======================================================================================
;;Returns a map containing a list of times and a list of values at the time
;;The nth value in times corresponds to the nth value in y, ie xy-pair = [(nth times i) (nth y i)]
;;The global-start/end values need to be calculated from the entire data set
;;times and y should ALWAYS be the same length
(comment
  (defn coords [map-list global-start global-end ykeyfn startkeyfn endkeyfn]
    (let [times (range global-start global-end)
          y (map #(y-at-time map-list % ykeyfn startkeyfn endkeyfn) times)]
      {:times times :y y}))
  
  ;;Returns a list of maps with groupkey as key, times as list of times and y-values as list of y-values at time
  (defn get-plot-data [grouped-map-list key startkeyfn endkeyfn ykeyfn]
    (for [k (keys grouped-map-list) :let [start (global-start grouped-map-list startkeyfn) end (global-end grouped-map-list endkeyfn)]]
      (conj {(keyword key) k} (coords (get grouped-map-list k) start end ykeyfn startkeyfn endkeyfn))))
  
  ;;Repeates val n times
  (defn rep [val n]
    (map #(if (or true %) val) (range n)))
  
  ;;Returns a JFreeChartObject reperenting the sand chart
  ;;grouped-map-list is the result of calling group-by on the list of maps from the read-in data
  ;;key is the variable name from the input file that the data should be grouped by (String, not keyword)
  ;;startfn, endfn, and yfn are all functions that return numerical values when called on a data map
  ;;The optional cont keyword argument will determine in the chart should be continous (true) or discrete (false)
  (defn build-sand-chart [grouped-map-list key startfn endfn yfn & {:keys [title] :or {title ""}}]
    (let [data (get-plot-data grouped-map-list key startfn endfn yfn)
          start (global-start grouped-map-list startfn)
          end (global-end grouped-map-list endfn)
          keys (keys grouped-map-list)
          length (count (:times (first data))) ;should be equal to end-start
          groupby (flatten (map #(flatten (if (or true %) keys)) (range length)))
          times (flatten (for [t (range start end)] (rep t (count keys))))
          quantities (flatten (for [t (range 0 length)] (map #(nth (:y %) t) data)))
          chart (if (= true true)
                  (incanter.charts/stacked-area-chart times quantities :group-by groupby 
                    :legend true :x-label "Time Period" :y-label "Quantity" :title title)
                  (incanter.charts/stacked-bar-chart times quantities :group-by groupby 
                    :legend true :x-label "Time Period" :y-label "Quantity" :title title))
          invisible (java.awt.Font. "Tahoma" 0 0)
          visible (java.awt.Font. "Tahoma" 0 8)]
      (.setMaximumCategoryLabelLines (.getDomainAxis (.getCategoryPlot chart)) 5) ;;can change for readability 
      (doseq [i (range start (inc end))]
        ;;need to set some of the tick label to be invisible to make it more readable
        ;;if the label overlap, they will get replaceced by "..."
        (if (zero? (rem i (Math/floor (/ end 20)))) ;Maximum of 20 label tick points, can change if need more
          (.setTickLabelFont (.getDomainAxis (.getCategoryPlot chart)) i visible)
          (.setTickLabelFont (.getDomainAxis (.getCategoryPlot chart)) i invisible)))
      chart)))
;;; =================================================================================================================

