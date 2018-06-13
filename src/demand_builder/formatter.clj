(ns demand_builder.formatter
    (:require [spork.util table parsing io]))

;;A name space to refactor demand_builder (core)

;;Period length in FORGE files is 8 days
(def periodlength 8)

;;Will convert a string into a number represntation. 
;;Can handle quotations and commas 
(defn read-num [string]
  (let [parse-num (:number spork.util.parsing/parse-defaults)
        custom-num #(parse-num (clojure.string/replace (clojure.string/replace % "," "") "\"" ""))]
    (custom-num string)))

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

;;Takes a single line from the :data list and the phase map from :phases (returned from read-forge)
;;Any blank or lines that do not contain individual data (No SRC value or aggregated/total values)
;;Expands each line into multiple lines split by a change in phase or change in quantity (by SRC)
;;Also fills in the remaining field not part of the raw input (Opertion/Phase, Duration, ect)
;;Returns a list of maps with the keys :Quantity, :StartDay, :Duration, :Operation, :Strength, :SRC, and :Title (SRC title)
(defn ->record [line phases]
  (let [times (sort (map first (filter #(number? (first %)) line)))
        get-phase (fn [phases time]
                      (last (filter identity (for [p (sort-by #(second %) phases)] (if (<= (second p) time) (first p))))))
        last-phase (last (sort phases))]
    (filter #(not (zero? (:Quantity %)))
      (for [t times] {:Quantity (if (= "" (get line t)) 0 (read-num (get line t)))
                      :StartDay t 
                      :Duration periodlength
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
    (flatten (map collapse by-phase))))

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

;;Removes classification identifier [ (U) or (S) ] from infront of force code label [(U) SE-99] -> [SE-99]
(def classiflabel #"\([A-Za-z]\) ")
(defn get-fc [string] (clojure.string/replace string classiflabel ""))

;;Expected header for the Vigentte Consolidated file (in any order)
;;ForceCode can contain classification identifier (U) or (S)
;; ***Required columns -- :ForceCode, :SRC, :Title, :Strength, :Quantity, :Titlte10_32
;; **Can contain additional columns which will be ignored.
;; **Header must match exactly (assert error will be thrown) 
(def vignette-schema {:ForceCode get-fc :SRC :text :Title :text :Strength :number :Quantity :number :Title10_32 :text})

;;Expected header for the Mapping file (in any order)
;;ForceCode can contain classification identifier (U) or (S), which will be removed
;; ***Required columns -- :ForceCode, :StartDay, :Duration 
;; **Can contain additional colulmns which will just be ignored 
;; ****ForceCodes must match EXACTLY to what is listed in the Vignette Consolidated file and FORGE filenames
(def mapping-schema {:ForceCode get-fc :StartDay :number :Duration :number})

;;Takes a list of maps (of FORGE data), the last phase for the FORGE scenario, and the final day for the scenario from the map (Start + Duration from map)
;;For the last period of each record, if the phase matches last phase, will change the duration to match the ending time specified by the map
;; **This can either increase or decrease the initial duration
;;Returns a list of maps (of FORGE data) that has been syncronized to end on the same day as what is listed in the map file.
(defn sync-map [forgerecords lastphase mapend]
  (let [splitrecs (partition-by :SRC (filter #(= lastphase (:Operation %)) forgerecords))
        lastrecs (map #(last (sort-by :StartDay %)) splitrecs)
        adjusted (map #(assoc % :Duration (+ (:Duration %) (- mapend (:StartDay %) (:Duration %)))) lastrecs)]
    (flatten (conj adjusted (into [] (clojure.set/difference (set forgerecords) (set lastrecs)))))))

;;Reads the mapfile and joins the appropriated data from either the Vignette Consolidated file or FORGE file.
;;For each Vignette in the map, joins timing data from map with quantity information from vignette consolidated file
;;For each Scenario, looks for the correponding Forge file and adds the start day offset and synconized to the map
;; ***If there is not corresponding FORGE file found, will throw error. ONLY include scenarios that have an existing file
;; ***The file name (FORGE_SE-XXXX) has to match what is in the Mapping file
;;Returns a list of maps will the data needed to build the demand record
(defn join-by-map [mapfile vignettefile]
  (let [map-data (try
                   (into [] (spork.util.table/tabdelimited->records mapfile :schema mapping-schema))
                   (catch java.lang.AssertionError e (throw (Exception. (str "Error reading MAP file (" mapfile ")\n" (.getMessage e))))))
        vignette-data (try
                        (into [] (spork.util.table/tabdelimited->records vignettefile :schema vignette-schema))
                        (catch java.lang.AssertionError e (throw (Exception. (str "Error reading CONSOLIDATED file (" vignettefile ")\n" (.getMessage e))))))
        scenario? (fn [string] (= "SE" (subs string 0 2))) ;;apply str, take n does not throw index out of bounds error if nil/length less than range (but should not be nil here ever)
        scenarios (filter scenario? (map :ForceCode map-data))
        vignettes (filter #(not (scenario? %)) (map :ForceCode map-data))
        filter-fc (fn [fc data] (filter #(= fc (:ForceCode %)) data))
        scenario-offset (fn [fc map-data] (:StartDay (first (filter-fc fc map-data))))
        scanario-mapend (fn [fc map-data] (let [m (first (filter-fc fc map-data))] (+ (:StartDay m) (:Duration m))))
        forgepath (fn [fc] (clojure.string/replace mapfile (spork.util.io/fname mapfile) (str "FORGE_" fc ".txt")))
        joined-vignettes (map #(conj (first (filter-fc % map-data)) (first (filter-fc % vignette-data))) vignettes)
        joined-forges (for [forge scenarios 
                            :let [mapend (scanario-mapend forge map-data)
                                  forgefile (forgepath forge)
                                  
                                  forgedata (try
                                              (forgefile->records forgefile)
                                              (catch Exception e (throw (Exception. (str "File not found for FOREGE_" forge "\n" (.getMessage e))))))
                                  
                                  phases (:phases (read-forge forgefile))
                                  offset (scenario-offset forge map-data)]]
                        (sync-map (map #(assoc % :StartDay (+ offset (:StartDay %))) forgedata) (first (last (sort phases))) mapend))]
    (flatten (conj joined-forges joined-vignettes))))

;;Fields in Demand files
(def demand-fields [:Type :Enabled :Priority :Quantity :DemandIndex :StartDay :Duration :Overlap
                    :SRC :SourceFirst :DemandGroup :Vignette :Operation :Category :Title10_32 
                    :OITitle :Strength])

;;Formatt Vignettes and FORGE maps as Demand data maps
(defn record->demand-record [{:keys [Quantity Duration StartDay SRC ForceCode Title
                                     Strength Vignette Operation Title10_32]}]
 {:Type     "Demand" ;;Type - 0  
  :Enabled   true ;;Enabled - 1  
  :Priority  1 ;;Priority - 2  
  :Quantity  Quantity ;;Quantity (of SRCs) - 3  
  :DemandIndex  1 ;;DemandIndex - 4  
  :StartDay StartDay ;;StartDay - 5  
  :Duration Duration ;;Duration - 6  
  :Overlap  45 ;;Overlap - 7   
  :SRC SRC     ;;SRC - 8 
  :SourceFirst "Uniform" ;;SourceFirst - 9  
  :DemandGroup  (or Vignette (if (= "S-" (subs ForceCode 0 2)) ForceCode "UnGrouped")) ;;DemandGroup - 10  
  :Vignette  (or Vignette ForceCode) ;;Vignette - 11  
  :Operation (or Operation ForceCode) ;;Operation - 12  
  :Category  "Rotational" ;;Category - 13  
  :Title10_32 (or Title10_32 "10") ;;Title10_32 - 14 
  :OITitle Title ;;OITitle - 15  
  :Strength Strength}) ;;Strength - 16  

;;From the root directory, finds the needed files, reads and formatts the data, then writes demand to an output file. 
;;***All needed files should be contained within the same directory
;;***The map file has the name root/MAP_[filename]
;;***The vignette consolidated file has the name root/CONSOLIDATED_[filename]
;;***Each FORGE file has the name root/FORGE_SE-XXXX (exactly matching FORCE CODE in MAP)
;; All files need to be text tab delimted (.txt) files, NOT EXCEL FILES
;; - The output file will be saved in the given root directory as root/[Subroot]-DEMAND.txt
(defn root->demandfile [root]  
  (let [isFile? (fn [ftype fnames] (filter #(clojure.string/includes? % ftype) fnames))
        files (map str (spork.util.io/list-files root))
        mapfile      (first (isFile? "MAP_" files))
        vignettefile (first (isFile? "CONSOLIDATED_" files))
        forgefiles   (isFile? "FORGE_" files)
        outfile      (str root (spork.util.io/fname root) "_DEMAND.txt")]
    (-> (->> (join-by-map mapfile vignettefile)
             (map record->demand-record)
             (sort-by (juxt :Vignette :SRC :StartDay)))     
        (spork.util.stream/records->file outfile :field-order demand-fields))
    (flatten (vector mapfile vignettefile forgefiles))))

