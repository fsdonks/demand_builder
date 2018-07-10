(ns demand_builder.forgeformatter
  (:require [spork.util.table]))

;;ns for reading data from FORGE file and parsing the data needed for a MARATHON demand record/
;;includes multiple methods of parsing using different formatts.

;;Period length in FORGE files is 8 days
(def periodlength 8)

;;Will convert a string into a number represntation. 
;;Can handle quotations and commas 
(defn read-num [string]
  (let [parse-num (:number spork.util.parsing/parse-defaults)
        custom-num #(parse-num (clojure.string/replace (clojure.string/replace % "," "") "\"" ""))]
    (custom-num string)))

;; ========== FUNCTIONS FOR PARSING DATA FROM THE SRC_BY_DAY SHEET (no tabular) ========================================================

;;Reads a forge file and formatts the input
;;The first line of the FORGE file is assumeed to be the phase header where the phase label is directly above the starting time period
;;The second line of the FORGE file is assumed to be the header including the time periods.
;;  each time period should be formatted as Day xxxxTP x, where x is a number. 
;;  *****When converting file from Excel to text tab delimited, REMOVE NEWLINES FROM ALL CELLS, 
;;  ***** FORGE files tend to have the time period formatted as Day xxxx\nTP x (new line within a cell)
;;  ***** NEWLINE values within any cell need to be removed for the file to be correctly read. 
;;A map will be returned with three keys, :header, :phases, and :data
;;-The value for :header will be the list of column names in the file
;;  **The expected header for FORGE files contains a field for: SRC Title Strength and time periods as "Day 0001TP 1" "Day 0009TP 2" ...
;;   -the ordering of the header should not matter and addititional fields can exist.
;;   -HOWEVER, the SRC, Title, Strength, and period headers need to be formatted exactly as listed. 
;;-The value for :phases will be a map with the phase name as the key and the start day of the phase as the value
;; **the phase start days correspond to the day (listed in header) and only needs to be adjusted by the map offset
;;-The value for :data will be a list of maps with the keys as the keys from the header and the values as the values in the corresponding column
;; **The keys for days have already been formatted and read as number. To get the quantity at time 8 for map m, use (get m 8)
(defn read-forge [filename]
  (with-open [r (clojure.java.io/reader filename)]
    (let [formatter #(if (and (clojure.string/includes? % "TP") (clojure.string/includes? % "Day"))
                       (read-num (clojure.string/replace (first (clojure.string/split % #"TP")) "Day " "")) %)
          phases (clojure.string/split (first (line-seq r)) #"\t")
          header (map formatter (clojure.string/split (first (line-seq r)) #"\t"))
          h (count (filter #(not (number? %)) header))
          formatted-phases (apply conj (map #(hash-map (first %) (second %))
                                         (filter #(not= "" (first %)) (zipmap (drop h phases) (sort (filter number? header))))))
          data (map #(clojure.string/split % #"\t") (into [] (line-seq r)))
          formatted-data (map #(zipmap header %) (filter #(and (>= (count %) h) (not= "" (first %))) data))]
      {:header header :phases formatted-phases :data formatted-data})))

;;Gets phase durting time
(defn get-phase [phases time]
 (last (filter identity (for [p (sort-by #(second %) phases)] (if (<= (second p) time) (first p))))))

;;Determins the current and next phase
;;Returns the difference between the two phases
(defn get-duration [phases time]
  (let [phase (get-phase phases time)
        times (map #(get phases %)
                (map first (partition-by #(= phase %) (map first (sort-by second phases)))))]
    (- (second times) (first times))))

;;Takes a single line from the :data list and the phase map from :phases (returned from read-forge)
;;Any blank or lines that do not contain individual data (No SRC value or aggregated/total values)
;;Expands each line into multiple lines split by a change in phase or change in quantity (by SRC)
;;Also fills in the remaining field not part of the raw input (Opertion/Phase, Duration, ect)
;;Returns a list of maps with the keys :Quantity, :StartDay, :Duration, :Operation, :Strength, :SRC, and :Title (SRC title)
(defn ->record [line phases]
  (let [times (sort (map first (filter #(number? (first %)) line)))
        last-phase (last (sort phases))]
    (filter #(not (zero? (:Quantity %)))
      (for [t times] {:Quantity (if (= "" (get line t)) 0 (read-num (get line t)))
                      :StartDay t 
                      :Duration (get-duration phases t)
                      :Operation (get-phase phases t)
                      :Strength (read-num (get line "Strength")) 
                      :SRC (get line "SRC")
                      :Title (get line "Title")}))))

(defn collapse [records & {:keys [formatted]}]
  (let [r (sort-by :StartDay records)]
    (if (<= (count records) 1)
      (conj formatted (first r))
      (if (= (:Quantity (first r)) (:Quantity (second r)))
        (collapse (conj (drop 2 r) (assoc (first r) :Duration (+ (:Duration (first r)) (:Duration (second r))))) :formatted formatted)
        (collapse (drop 1 r) :formatted (conj formatted (first r)))))))

;;Takes a list of formatted records and reduces where possible.
;;A record is reduced if the Vignette, SRC, Operation/Phase, and Quantity are all equal. 
;;If a record is to be reduced, all values remain unchanged except for duration which is the sum of the two records.
;;Returns a list of maps with the keys :Quantity, :StartDay, :Duration, :Operation, :Strength, :SRC, and :Title (SRC title)
(defn reduce-records [records]
  (let [by-phase (partition-by :Operation (sort-by #(vector (:StartDay %) (:Quantity %)) records))]
    (filter #(and (> (:Duration %) 0) (not= 0 (:Quantity %)) (not= nil (:Quantity %))) (flatten (map collapse by-phase)))))

;;Returns a list of maps with the keys :DemandGroup, :Vignette, :Quantity, :StartDay, :Duration, :Operation, :Strength, :SRC, and :Title (SRC title)
;;Uses read-forge, ->record, and reduce-records to build the inital map list
;;From the filename, determines the Vignette and DemandGroup (ForceCode) 
;; ***It is assumed that FORGE files have the formatting of root-path/FORGE_SE-xxxx.txt, where SE-xxxx is the scenario force code.
;; **** The ForceCode determined by the file name has to EXACTLY match what is listed as the ForceCode in the Vignette Mapping file.
(defn forgefile->records [filename]
  (let [vignette (clojure.string/replace (last (clojure.string/split filename #"FORGE_")) (str "." (spork.util.io/fext filename)) "")
        input (read-forge filename)
        last-phase (last (sort (:phases input)))]
    (for [m (flatten (map #(reduce-records (->record % (:phases input))) (:data input)))]
      (-> (assoc m :DemandGroup vignette) (assoc :Vignette vignette)))))


;; =====================================================================================================================================
;; =====================================================================================================================================

;; ========== FUNCTIONS FOR PARSING DATA FROM UNIT_NODE_DETAIL (tabular) SHEET FROM FORGE ==============================================
(def forge-schema {(keyword "UIN Quantity") :number
                   (keyword "Time Period Begin Day") :number
                   (keyword "Time Period Days") :number
                   :Subphase :text
                   (keyword "SRC Strength") :number
                   :SRC :text
                   :Title :text})

;;Rename Fields in map
(defn forge->map [forgefile]
  (let [data (into [] (spork.util.table/tabdelimited->records forgefile :schema forge-schema))]
    (map #(-> (assoc % :Quantity (get % (keyword "UIN Quantity")))
              (assoc :StartDay (get % (keyword "Time Period Begin Day")))
              (assoc :Duration (get % (keyword "Time Period Days")))
              (assoc :Operation (:Subphase %))
              (assoc :Strength (get % (keyword "SRC Strength")))
              (assoc :Vignette (clojure.string/replace (last (clojure.string/split forgefile #"FORGE_")) ".txt" ""))) 
      data)))

;;Adds durations if SRC, Phase (operation), and Quantity are equal
(defn merge-duration [records]
  (let [data (sort-by #(vector (:SRC %) (:StartDay %) (:Quantity %)) records)
        by-phase (partition-by #(vector (:SRC %) (:Operation %) (:Quantity %)) data)]
    (map #(assoc (first %) :Duration (reduce + (map :Duration %))) by-phase)))

(defn forge->records [forgefile]
  (sort-by #(vector (:SRC %) (:StartDay %)) (merge-duration (forge->map forgefile))))

(defn last-phase [records]
  (:Operation (last (sort-by :StartDay records))))

