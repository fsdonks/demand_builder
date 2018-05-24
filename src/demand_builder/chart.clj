(ns demand_builder.chart
   (:require [clojure.java [io :as io]]
             [spork.util [io :refer [list-files fpath fname fext write! writeln!]]]
             [incanter core charts stats datasets]))

;;Name space for formatting charts using data from demand builder output

(defn read-num [string]
  (if string
    (let [num (fn [string] (apply str (map #(re-matches #"[\d.]*" %) (map str string))))
          n (clojure.string/split (num string) #"[.]")  t (take 2 n) b (drop 2 n)
          d (read-string (str (first t) "." (second t) (apply str b)))]
      (if (zero? (- d (int d))) (int d) d))
    0))

(defn read-formatted-demand [filename]
  (with-open [r (clojure.java.io/reader filename)] 
       (let [firstline (clojure.string/split (first (line-seq r)) #"\t")
             header (zipmap (range (count firstline)) firstline)
             lines (map #(clojure.string/split % #"\t") (line-seq r)) ;;header alread read, no need to drop
             data (for [line lines]
                    (zipmap (map keyword firstline) line))]
         (group-by #(get % :Vignette) data))))
         
(defn vignette-start [vdata]
  (apply min (map #(read-num (:StartDay %)) vdata)))

(defn vignette-end [vdata]
  (apply max (map #(+ (read-num (:Duration %)) (read-num (:StartDay %))) vdata)))

(defn in-period [vdata start end]
  (filter #(and (>=  start (read-num (:StartDay %))) 
                (<= end (+ (read-num (:Duration %)) (read-num (:StartDay %)))))
           vdata))

(defn total-at-time [vdata time]
  (apply + (map #(* (read-num (:Quantity %)) (read-num (:People %))) (in-period vdata time (inc time)))))

(defn coords [vdata global-start global-end]
  (let [times (range global-start global-end)
        quantity (map #(total-at-time vdata %) times)]
    {:times times :quantities quantity}))

(defn global-start [data]
  (apply min
    (for [d data]
      (vignette-start (second d)))))

(defn global-end [data]
  (apply max
    (for [d data]
      (vignette-end (second d)))))

;;Returns a list of maps with the Vignette, sequence of time periods it is active, and aggregate quantity at the corresponding time
(defn get-plot-data [data]
  (for [k (keys data) :let [start (global-start data) end (global-end data)]]
    (conj {:Vignette k} (coords (get data k) start end))))

;;OLD - will create histogram of data, for sand chart (stacked chart) use build-chart
(defn sand-chart [vdata]
  (incanter.charts/time-series-plot (:times vdata) (:quantities vdata)))

;;OLD - will create histogram of data, for sand chart (stacked chart) use build-chart
(defn sand-charts [data & {:keys [title] :or {title ""}}]
  (let [first-plot (first data)
        chart (incanter.charts/time-series-plot (:times first-plot) (:quantities first-plot)
                :title title :x-label "Time" :y-label "Quantity" :legend true :series-label (:Vignette first-plot))]
    (doseq [d (drop 1 data)]
      (incanter.charts/add-lines chart (:times d) (:quantities d) :series-label (:Vignette d)))
    chart))



;(incanter.core/view (incanter.charts/stacked-area-chart [1 1 1 2 2 2 3 3 3] [10 20 30 15 25 35] :group-by [1 2 3 1 2 3] :legend true))

;;Repeates val n times
(defn rep [val n]
  (map #(if (or true %) val) (range n)))

;;cont option determins in time between periods is continous or discrete
;;If cont is true, will build a stacked-area-chart, otherwise, will build a stacked-bar-chart
(defn build-chart [data & {:keys [title cont] :or {title "" cont true}}]
  (let [start (apply min (map #(apply min (:times %)) data))
        end (apply max (map #(apply max (:times %)) data))
        keys (map #(:Vignette %) data)
        length (count (:times (first data)))
        groupby (flatten (map #(flatten (if (or true %) keys)) (range length)))
        times (flatten (for [t (range start (inc end))] (rep t (count keys))))
        quantities (flatten (map #(:quantities %) data))
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
(defn demand-file->sand-charts [filename & {:keys [save view cont] :or {save false view true cont true}}]
  (let [prefix (first (clojure.string/split (last (clojure.string/split filename #"[/|\\]")) #"[.]"))
        chart (build-chart (get-plot-data (read-formatted-demand filename)) :cont cont :title (str prefix "- Sand Chart"))]
    (when save
      (incanter.core/save chart (str  filename "-SandChart.png")))
    (when view
      (incanter.core/view chart))
    chart))


