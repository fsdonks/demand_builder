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
         ;(println firstline)
         ;(doseq [d data] (identity d))
         (group-by #(get % :Vignette) data))))
         
(defn vignette-start [vdata]
  (apply min (map #(read-num (:StartDay %)) vdata)))

(defn vignette-end [vdata]
  (apply max (map #(+ (read-num (:Duration %)) (read-num (:StartDay %))) vdata)))

(defn in-period [vdata start end]
  (filter #(and (>=  start (read-num (:StartDay %))) 
                (<= end (+ (read-num (:Duration %)) (read-num (:StartDay %)))))
           vdata))

;;; ***NO QUANTITY VARIABLE IN DEMAND FILE, USING MANUALLY ADDED PEOPLE FOR TESTING ***
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

;; *** ADDED PEOPLE FIELD IN TEST DATA, REAL DEMAND FILE HAS NO QUANTITY VALUES, WILL NEED TO BE LOOKED UP ***
;;Returns a list of maps with the Vignette, sequence of time periods it is active, and aggregate quantity at the corresponding time
(defn get-plot-data [data]
  (for [k (keys data) :let [start (global-start data) end (global-end data)]]
    (conj {:Vignette k} (coords (get data k) start end))))

(defn sand-chart [vdata]
  (incanter.charts/time-series-plot (:times vdata) (:quantities vdata)))

(defn sand-charts [data & {:keys [title] :or {title ""}}]
  (let [first-plot (first data)
        chart (incanter.charts/time-series-plot (:times first-plot) (:quantities first-plot)
                :title title :x-label "Time" :y-label "Quantity" :legend true :series-label (:Vignette first-plot))]
    (doseq [d (drop 1 data)]
      (incanter.charts/add-lines chart (:times d) (:quantities d) :series-label (:Vignette d)))
    chart))

;;Function to build sand chart from formatted demand file
;;Save will save the chat as a png with the filename filename-SandChart.png in the same directory as the original file
;;View will create a JFrame and set it as visible
;;Returns JFreeChart object
(defn demand-file->sand-charts [filename & {:keys [save view] :or {save false view true}}]
  (let [prefix (first (clojure.string/split (last (clojure.string/split filename #"[/|\\]")) #"[.]"))
        chart (sand-charts (get-plot-data (read-formatted-demand filename)) :title (str prefix "-SandChart"))]
    (when save
      (incanter.core/save chart (str  filename "-SandChart.png")))
    (when view
      (incanter.core/view chart))
    chart))


