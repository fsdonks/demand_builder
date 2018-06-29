(ns demand_builder.chart
  (:require [incanter core charts]
            [spork.util table parsing]
            [demand_builder [jfreeoverride :as jf]]))


;;Name space for Stacked chart generation using temporal data
;;Genic functionality for re-usability

;;Specific function to create sand charts for demand from file 
;;  ***uses specific file format, works as of May 2018***
;;     changes to demand file will require (ONLY) demand-file->sand-charts to be updated (update schema)

;;Function to parse strings as ints (can handle commas)
(defn read-num [string]
  (let [parse-num (:number spork.util.parsing/parse-defaults)
        custom-num #(parse-num (clojure.string/replace % "," ""))]
    (custom-num string)))

;;Returns a list of maps containing the data from the tab-delimited file
;;The keys of each map are either the header row of the file if no schema is provided
;;Otherwise, the keys of each map are what are defined by the schema and are formatted accordingly
(defn file->map-list [filename & {:keys [schema] :or {schema {}}}]
  (as-> filename it
        (spork.util.table/tabdelimited->records it :schema schema)
        (into [] it)))

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
                              & {:keys [title x-label y-label schema] :or {title "" x-label "Time" y-label "Quantity" schema {}}}]
  (let [ds (jf/->ds (file->map-list filename :schema schema) groupby-key yfn startfn endfn)
        chart (jf/stacked-xy-chart ds :title title :x-label x-label :y-label y-label)]
    chart))

;;Example Colors for charts 
(def colors {"HLD" :blue "FA/CT" :red "Det" :green})

;;Function to build sand chart from formatted demand file
;;Save will save the chat as a png with the filename filename-SandChart.png in the same directory as the original file
;;View will create a JFrame and set it as visible
;;Cont will make the changes between time periods continous, otherwise the changes will be discrete
;;Returns JFreeChart object
(defn file->sand-charts [filename key startfn endfn yfn 
                         & {:keys [save view title x-label y-label schema color-map ordering] 
                            :or {save false view true title "Sand Chart" x-label "Time" y-label "Quantity" schema {} color-map colors ordering nil}}]
  (let [prefix (clojure.string/replace (spork.util.io/fname filename) (str "." (spork.util.io/fext filename)) "")
        outfile (clojure.string/replace filename (str "." (spork.util.io/fext filename)) "-SandChart.png")
        chart (file->stacked-xy-chart filename key startfn endfn yfn 
                :save save :view view :title (str prefix " - Sand Chart") :x-label x-label :y-label y-label :schema schema)] 
    (when save (incanter.core/save chart outfile))
    (when view (incanter.core/view chart))
    (jf/set-xysand-colors (if ordering (jf/sort-series chart ordering)) color-map)))

;;; =============== FUNCTIONS SPECIFIC FOR DEMAND FILE SAND CHARTS ================



(defn first-row
  "Returns the first row of a file."
  [path]
  (spork.util.general/first-any (spork.util.general/line-reducer path)))

(defn header-exists?
  "Returns true if the header exists in the file."
  [path header & {:keys [sep] :or {sep #"\t"}}]
  (-> (first-row path)
      (clojure.string/split sep)
      (set)
      (contains? header)))

;;Function for Sand Charts from formatted demand file
;;If demand file does not contain strength (people) per src, a supply file can be supplied to pull the data from.
;;We can probably delete the fromSupply schema below.  Even in the first branch of if, :schema should likely be
;;Fromdemand.

(defn demand-file->sand-charts
  ":ordering will also filter demandgroups. :color-map only works when :ordering is specified."
  [demandfile & {:keys [save view cont supplyfile color-map ordering] :or {save true view false cont true ordering nil
                                                                           color-map nil}}]
  (let [startfn #(:StartDay %)
        endfn #(+ (:StartDay %) (:Duration %))
        dschema {:SRC :text :DemandGroup :text :Quantity read-num :StartDay read-num :Duration read-num :Strength read-num}
        fromDemand (if (header-exists? demandfile "Strength") dschema (dissoc dschema :Strength))
        fromSupply {:SRC :text :DemandGroup :text :Quantity read-num :StartDay read-num :Duration read-num :Strength read-num}
        chart (if (not= nil supplyfile)
                (let [supply (set (for [m (file->map-list supplyfile)] [(:SRC m) (:Strength m)]))
                      strmap (into {} supply)]
                  (file->sand-charts demandfile "DemandGroup" startfn endfn #(* (:Quantity %) (max 1 (get strmap (:SRC %) 1)))
                    :ordering ordering :color-map color-map :schema fromSupply :view view :save save))
                (file->sand-charts demandfile "DemandGroup" startfn endfn #(* (:Quantity %) (max 1 (get % :Strength 1))) 
                  :ordering ordering :color-map color-map :schema fromDemand :view view :save save))]
    chart))
;;; ===============================================================================

;;Example usage using color and order formatting
;;(demand_builder.chart/demand-file->sand-charts f :view true :color-map {"SE-99" (java.awt.Color/red) "S-706" (java.awt.Color/blue) "S-7337" (java.awt.Color/green)} :ordering ["SE-99" "S-7337" "S-706"])
