(ns demand_builder.forgeformatter
  (:require [spork.util.table :as tbl]
            [spork.util.excel.docjure :as docjure]
            [spork.util.excel.core :as xl]
            [clojure.string :as str]
            [clojure.java.io :as jio]
            [spork.util.io :as io]))

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
;; **The keys for days have already been formatted and read as
;;number. To get the quantity at time 8 for map m, use (get m 8)
;;save the SRC_by_day worksheet as tab delimitted text for demand
;;builder
(defn remove-newlines-split [filename]
  (-> ;;remove newlines from cells.
   (str/replace filename #"\nTP" "TP")
   (str/split-lines)))

(defn read-and-strip
  "Turn a cell into a string with read-cell but also put the cell
  values that have a newline in them in quotes so that it opens as tab
  delimited in Excel properly."
  [c]
  (let [v (docjure/read-cell c)]
    (when v
      (clojure.string/replace v #"\n" ""))))
       
(defn forge-wkbk->str
  "Return the string representation of an src_by_day worksheet.
  Expect SRC_By_Day to be a worksheet in the already loaded
  forge-wkbk."
  [forge-wkbk]
  (->> forge-wkbk
       (docjure/select-sheet "SRC_By_Day")
       docjure/row-seq
       (map (fn [x] (if x (docjure/cell-seq x))))
       (map #(reduce str (interleave
                          (map (fn [c]
                                 (read-and-strip c)) %)
                          (repeat "\t"))))
       ((fn [x] (interleave x (repeat "\n"))))
       ;;One extra newline to remove at the end.
       (butlast)
       (reduce str)))

(defn forge-by-day-from
  "Allows loading the forge data from the path to an src-by-day text
  file or from an Excel workbook."
  [forge-path]
  (if (.exists (jio/as-file forge-path))
    (let [extension (.toLowerCase (io/fext forge-path))]
      (case extension
        "txt" 
        (slurp forge-path)
        "xlsx"
        (forge-wkbk->str
        ;;check for a resource first
        (if (jio/resource forge-path)
          (docjure/load-workbook-from-resource forge-path)
          (xl/as-workbook forge-path)))))
    (throw (ex-info "FORGE file doesn't exist." {:file-path
                                                 forge-path}))))
    
(defn splitted-forge [forge-path]
  (remove-newlines-split (forge-by-day-from forge-path)))
  
(defn read-forge [filename]
  (let [l (splitted-forge filename)
        formatter #(if (and (str/includes? % "TP") (str/includes? % "Day"))
                       (read-num (str/replace (first (str/split % #"TP")) "Day " "")) %)
          phases (str/split (first l) #"\t")
          header (map formatter (str/split (second l) #"\t"))
          h (count (filter #(not (number? %)) header))
          formatted-phases (apply conj (map #(hash-map (first %) (second %))
                                         (filter #(not= "" (first %)) (zipmap (drop h phases) (sort (filter number? header))))))
          data (map #(str/split % #"\t") (into [] (drop 2 l)))
          formatted-data (map #(zipmap header %) (filter #(and (>= (count %) h) (not= "" (first %))) data))]
      {:header header :phases formatted-phases :data formatted-data}))

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

(defn get-duration [t times]
  (let [t2 (drop-while #(not= t %) times)]
    (if (second t2) (- (second t2) t) 8)))

(defn get-previous
  "returns the previous day"
  [t times]
  (last (take-while #(not= t %) times)))

(defn sort-by-vals
  "takes a map and sorts it by the values."
  [m]
  (into (sorted-map-by (fn [k1 k2] (compare (m k1) (m k2)))) m))

(defn forge-quantity
  "Round the forge number up in case of
  fractional units required."
  [quantity]
  (int (Math/ceil quantity)))

;;Takes a single line from the :data list and the phase map from :phases (returned from read-forge)
;;Any blank or lines that do not contain individual data (No SRC value or aggregated/total values)
;;Expands each line into multiple lines split by a change in phase or change in quantity (by SRC)
;;Also fills in the remaining field not part of the raw input (Opertion/Phase, Duration, ect)
;;Returns a list of maps with the keys :Quantity, :StartDay, :Duration, :Operation, :Strength, :SRC, and :Title (SRC title)
(defn ->record [line phases]
  (let [;;we're returning the same times every time this fn is
        ;;called (inefficiency)
        times (sort (map first (filter #(number? (first %)) line)))
        forgestart (second (first (sort-by-vals phases)))
        startday (first times)]
    (filter #(not (zero? (:Quantity %)))
            (for [t times
                  :let [prev (get-previous t times)]]
        {:Quantity (if (= "" (get line t)) 0 (forge-quantity
                                              (read-string (get line t))))
         :StartDay (if prev (+ prev 1) 1)
         :Duration (if prev (- t prev) 1)
         :Operation (get-phase phases t)
         :Strength (read-num (get line "Strength")) 
         :SRC (get line "SRC")
         :Title (get line "Title")}))))

(defn collapse [records & {:keys [formatted]}]
  (let [r (sort-by #(vector (:Operation %) (:SRC %) (:StartDay %)) records)]
    (if (<= (count records) 1)
      (conj formatted (first r))
      (if (and (= (:SRC (first r) (:SRC (second r)))) (= (:Quantity (first r)) (:Quantity (second r))))
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
  (let [vignette (clojure.string/replace (last (clojure.string/split filename #"FORGE_")) (str "." (io/fext filename)) "")
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
  (let [data (into [] (tbl/tabdelimited->records forgefile :schema forge-schema))]
    (map #(-> (assoc % :Quantity (forge-quantity (get % (keyword "UIN Quantity"))))
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
  (sort-by #(vector (:SRC %) (:StartDay %)) (forge->map forgefile)))

(defn last-phase [records &  {:keys [mapend offset] :or {mapend nil offset nil}}]
  (let [res (if (and mapend offset)
              (:Operation (last (sort-by :StartDay (filter #(>= mapend (+ offset (:StartDay %) (:Duration %))) records))))
              (:Operation (last (sort-by :StartDay records))))]
    ;Covers the case where mapend is less than the end day of the very first phase record
    (if (empty? res) (:Operation (first (sort-by :StartDay records))) res)))


(defn read-header [file]
 (with-open [r (jio/reader file)]
   (clojure.string/split (first (line-seq r)) #"\t")))

(def unit-node-detail-header ["UIN Quantity" "Time Period Begin Day" "Time Period Days" "SRC Strength"])

;;Checks is sheet is Unit_Node_Detail sheet by checking for needed columns in unit-node-detail-header
(defn isUND? [file]
  (not= #{} (clojure.set/intersection (set (read-header file)) (set unit-node-detail-header))))

;;Can read either Unit_Node_Detail sheet or SRC_By_Day sheet
(defn any-forge->records [filename]
  (if (isUND? filename)
    (forge->records filename)
    (forgefile->records filename)))

(defn get-forge-phases [by-day-path]
  (:phases (read-forge by-day-path)))
              
(defn parse-phase [phase]
  (cond
    (str/includes? phase "FwdStation") "phase1"
    (str/includes? phase "PH IV") "phase4"
    (str/includes? phase "PH III") "phase3"
    (str/includes? phase "PH II") "phase2"
    ;;because the previous 3 include this one...
    (str/includes? phase "PH I") "phase1"   
    :else (throw (ex-info "No phase mapping exists."
                          {:phase phase}))))

(defn add-initial-phase [sorted-phases]
    (concat [["0-day" 0]] sorted-phases))

(defn end-pairs->bounds
  "Takes a partition of the FORGE phase-end map and returns the start
  and end of the second phase of the partition."
  [[[phase-1 end-1] [phase-2 end-2]]]
  [phase-2 (inc end-1) end-2])

(defn collapse-phase
  [[phase phase-tuples]]
  (let [start-day (second (first phase-tuples))
        end-day (last (last phase-tuples))]
    [phase start-day end-day]))
  
(defn collapse-phases
  "Takes a seq of [phase start end] tuples, parses the phase with
  phase-parser, and assumes that multiple tuples may now have the same
  phase, so collapses the start and end into one phase tuple.  Assumes
  that tuples with the same phase will be adjacent."
  [phase-tuples & {:keys [phase-parser] :or {phase-parser
                                             parse-phase}}]
  (->> phase-tuples
       (map #(update % 0 phase-parser))
       (group-by first)
       (map collapse-phase)))
  
(defn ends->inclusives
  "Turns FORGE phase-end map into a seq of phases starts and ends,
  using a phase-parser per collapse-phases."
  [phase-map & {:keys [phase-parser] :or {phase-parser
                                          parse-phase}
                :as opt-args}]
  (let [sorted-phases (sort-by second (seq phase-map))]
    (->> sorted-phases
         add-initial-phase
         (partition 2 1)
         (map end-pairs->bounds)
         (#(collapse-phases % opt-args)))))

(defn project-phases
  "Given phase tuples and an added-time, add the added time minus one
  to each start and end."
  [phase-tuples added-time]
  (let [project #(+ % (dec added-time))]
    (for [[phase start end] phase-tuples]
      [phase (project start) (project end)])))
  
(defn processed-phases-from
  "Given the path to a FORGE file, return the phase and start and end
  days for each phase after post processing with the phase-parser,
  projecting the times based on a start-day."
  [by-day-path & {:keys [phase-parser start-day] :or {phase-parser
                                                      parse-phase
                                                      start-day
                                                      1}
                  :as opt-args}]
  (-> (get-forge-phases by-day-path)
      (ends->inclusives opt-args)
      (project-phases start-day)))       
