(ns demand_builder.chart
  (:require [incanter core charts]
            [spork.util [table :as t]])
  (:import [org.jfree.data.xy DefaultTableXYDataset XYSeries]))
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
         lines (map #(clojure.string/split % (re-pattern del)) (line-seq r))
         data (for [line lines] (zipmap (map keyword firstline) line))]
     (into [] data))))

;;Groups data map-list by key (String)
;;Returns multi-demensional list of map-list
(defn group-by-key [map-list key]
  (group-by #(get % (keyword key)) map-list))

;;Returns set of records that fall into the given time periods
;;Map-list is a single instance of a map list (a single entry after calling group-by-key or an entry after file->map-list if ungrouped)
;;   Example: map-list = ({:key "k1" :start 0 :duration 100 :otherdata "data"} {:key "k2" :start 25 :duration :50 :otherdata "more data")
;; Start and end are the time period bondaries (record falls within period [start, end] inclusive)
;; startkeyfn and endkeyfn are functions that can be called on a single data map 
;;   and will return the field that represents the records start time/end time
;;   Example: startkeyfn = #(read-num (:StartDay %))
;;            endkeyfn = #(+ (read-num (:Duration %)) (read-num (:StartDay %)))
;; *startkeyfn and endkeyfn are expected to return numerical values (not strings)
(defn in-period [map-list start end startkeyfn endkeyfn]
  (filter #(and (>= start (startkeyfn  %)) (<= end (endkeyfn %))) map-list))

;;Returns the y value to be used for the record at x value (time) of time
;;Map list is a single instance of a map list (ex: ({:key "k1" :start 0 :duration 100 :otherdata "data"} {:key "k2" :start 25 :duration :50 :otherdata "more data"))
;;ykeyfn is a function that gets the y-value from a single record (map)
;;   Example: #(* (read-num (:Quantity %)) (read-num (:People %)))
(defn y-at-time [map-list time ykeyfn startkeyfn endkeyfn]
  (apply + (map ykeyfn (in-period map-list time (inc time) startkeyfn endkeyfn))))

;;Returns a map containing a list of times and a list of values at the time
;;The nth value in times corresponds to the nth value in y, ie xy-pair = [(nth times i) (nth y i)]
;;The global-start/end values need to be calculated from the entire data set
;;times and y should ALWAYS be the same length
(defn coords [map-list global-start global-end ykeyfn startkeyfn endkeyfn]
  (let [times (range global-start global-end)
        y (map #(y-at-time map-list % ykeyfn startkeyfn endkeyfn) times)]
    {:times times :y y}))

;;Calculates the local start time of the map-list
;;startkeyfn is a function that returns the start time from the map *as a numerical value (not string)
(defn local-start [map-list startkeyfn]
  (apply min (for [m map-list] (startkeyfn m))))

;;Calculates the local end time of the map-list
;;endkeyfn is a function that returns the end time from a map *as a numerical value (not string)
;;   Example: endkeyfn = #(+ (read-num (:Duration %)) (read-num (:StartDay %)))
(defn local-end [map-list endkeyfn]
  (apply max (for [m map-list] (endkeyfn m))))

;;Calculates the global start time by finding the minimum of all local start times
(defn global-start [grouped-map-list startkeyfn]
  (apply min
    (for [k (keys grouped-map-list) :let [map-list (get grouped-map-list k)]]
     (local-start map-list startkeyfn))))
    
;;Calculates the global start time by finding the minimum of all local start times
(defn global-end [grouped-map-list endkeyfn]
  (apply max
    (for [k (keys grouped-map-list) :let [map-list (get grouped-map-list k)]]
     (local-end map-list endkeyfn))))


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
(defn build-sand-chart [grouped-map-list key startfn endfn yfn & {:keys [title cont] :or {title "" cont true}}]
  (let [data (get-plot-data grouped-map-list key startfn endfn yfn)
        start (global-start grouped-map-list startfn)
        end (global-end grouped-map-list endfn)
        keys (keys grouped-map-list)
        length (count (:times (first data))) ;should be equal to end-start
        groupby (flatten (map #(flatten (if (or true %) keys)) (range length)))
        times (flatten (for [t (range start end)] (rep t (count keys))))
        quantities (flatten (for [t (range 0 length)] (map #(nth (:y %) t) data)))
        chart (if (= true cont)
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
    chart))

;;Function to build sand chart from formatted demand file
;;Save will save the chat as a png with the filename filename-SandChart.png in the same directory as the original file
;;View will create a JFrame and set it as visible
;;Cont will make the changes between time periods continous, otherwise the changes will be discrete
;;Returns JFreeChart object
(defn file->sand-charts [filename key startfn endfn yfn & {:keys [save view cont] :or {save false view true cont true}}]
  (let [prefix (first (clojure.string/split (last (clojure.string/split filename #"[/|\\]")) #"[.]"))
        chart (build-sand-chart (group-by-key (file->map-list filename) key) key startfn endfn yfn
                :cont cont :title (str prefix " - Sand Chart"))]
    (when save
      (incanter.core/save chart (str (apply str (take (- (count filename) 4) filename)) "-SandChart.png")))
    (when view
      (incanter.core/view chart))
    chart))

;;; =============== FUNCTIONS SPECIFIC FOR DEMAND FILE SAND CHARTS ================
;;Function for Sand Charts from formatted demand file
(defn demand-file->sand-charts [filename & {:keys [save view cont] :or {save false view true cont true}}]
  (file->sand-charts filename "Vignette" #(read-num (:StartDay %)) #(+ (read-num (:StartDay %)) (read-num (:Duration %))) #(read-num (:People %)) :save true)) 
;;; ===============================================================================


(defn distinct-times [map-list startfn endfn]
  (sort 
    (distinct 
      (apply conj
        (map startfn map-list)
        (map endfn map-list)))))

;;Returns map of xy-pairs with key as time and value as quantity (only includes distinct times)
(defn xy-pairs [map-list yfn startfn endfn]
  (let [dtimes (distinct-times map-list startfn endfn)]
    (zipmap dtimes (map #(y-at-time map-list % yfn startfn endfn) dtimes))))


(defn xyseries-list [grouped-map-list yfn startfn endfn] 
  (for [k (map first grouped-map-list) :let [m (get grouped-map-list k)]]
    (let [series (org.jfree.data.xy.XYSeries. k)]
      (doseq [xy (xy-pairs m yfn startfn endfn)]
        (.add series (first xy) (second xy)))
      series)))

(defn xydataset [grouped-map-list yfn startfn endfn]
  (let [ds (org.jfree.data.xy.DefaultTableXYDataset.)]
    (doseq [series (xyseries-list grouped-map-list yfn startfn endfn)]
      (.addSeries ds series))
    ds))



