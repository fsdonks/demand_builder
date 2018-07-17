(ns demand_builder.formatter
  (:require [spork.util table parsing io]
            [demand_builder [forgeformatter :as ff]]
            [clojure.java [io :as jio]]))

;;A name space to refactor demand_builder (core)

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

;;list of forge records that need to be adjusted to match the map
(def shifted-forges (atom []))

;;Filters data by FC
(defn filter-fc [fc data] (filter #(= fc (:ForceCode %)) data))


(defn shorten-recs [recs mapend lastphase]
  (filter #(and (= lastphase (:Operation %)) (<= mapend (+ (:StartDay %) (:Duration %)))) recs))

(defn extend-recs [recs mapend lastphase]
  (let [last-phase-recs (filter #(= lastphase (:Operation %)) recs)
        last-day-in-phase (apply max (map :StartDay last-phase-recs))]
    (filter #(= last-day-in-phase (:StartDay %)) last-phase-recs)))

;;Takes a list of maps (of FORGE data), the last phase for the FORGE scenario, and the final day for the scenario from the map (Start + Duration from map)
;;For the last period of each record, if the phase matches last phase, will change the duration to match the ending time specified by the map
;; **This can either increase or decrease the initial duration
;;Returns a list of maps (of FORGE data) that has been syncronized to end on the same day as what is listed in the map file.
(defn sync-map [forgerecords lastphase mapend & {:keys [phases fc] :or {phases nil fc nil}}]
  (let [lastday (apply max (map #(+ (:StartDay %) (:Duration %)) (filter #(= lastphase (:Operation %)) forgerecords)))
        lastrecs (if (< mapend lastday) (shorten-recs forgerecords mapend lastphase) (extend-recs forgerecords mapend lastphase))
        adjusted (map #(assoc % :Duration (+ (:Duration %) (- mapend (:StartDay %) (:Duration %)))) lastrecs)
        _ (doseq [r (zipmap lastrecs adjusted)]
            (when (not= (:Duration (first r)) (:Duration (second r)))
              (swap! shifted-forges conj (first r))))] 
    (filter #(and (pos? (:Duration %)) (>= mapend (+ (:StartDay %) (:Duration %))))
      (flatten (conj adjusted (into [] (clojure.set/difference (set forgerecords) (set lastrecs))))))))

;;list of duplicate records in either forge or consolidated files
(def dup-recs (atom []))

;;When duplicate vignettes, add quantites - only when multiple rows with the same SRC
(defn reduce-cons [vignettes]
  (let [by-srcs (map second (group-by #(vector (:SRC %) (:ForceCode %)) (sort-by #(vector (:SRC %) (:StartDay %)) vignettes)))]
    (for [v by-srcs :let [quantity (apply + (map :Quantity v)) 
                          _ (when (> (count v) 1) (doseq [val v] (swap! dup-recs conj val)))]]
      (assoc (first v) :Quantity quantity))))

;;When duplicate forges, add quantites - only when same SRC and startDay
(defn reduce-forge [forges]
  (let [by-src-day (map second (group-by #(vector (:SRC %) (:StartDay %)) (sort-by #(vector (:SRC %) (:StartDay %)) forges)))]
    (for [v by-src-day :let [quantity (apply + (map :Quantity v)) 
                             _ (when (> (count v) 1) (doseq [val v] (swap! dup-recs conj val)))]]
      (assoc (first v) :Quantity quantity))))

;;Reads vignette file, throws more descriptive error
(defn read-vignette [vignettefile]
 (try
  (reduce-cons (into [] (spork.util.table/tabdelimited->records vignettefile :schema vignette-schema)))
  (catch java.lang.AssertionError e (throw (Exception. (str "Error reading CONSOLIDATED file (" vignettefile ")\n" (.getMessage e)))))))

;;Reads mapping file, throws more descriptive error
(defn read-mapfile [mapfile]
 (try
  (into [] (spork.util.table/tabdelimited->records mapfile :schema mapping-schema))
  (catch java.lang.AssertionError e (throw (Exception. (str "Error reading MAP file (" mapfile ")\n" (.getMessage e)))))))

;;Reads forgefile, throws more descriptive error
(defn read-forgefile [fc forgefile]
 (try
  (ff/merge-duration (reduce-forge (ff/any-forge->records forgefile)))
  (catch Exception e (throw (Exception. (str "File not found for FORGE_" fc "\n" (.getMessage e)))))))

;;Returns true when the fc string represents a scenario
(defn scenario? [string] (= "SE" (subs string 0 2))) 

;;Returns the starting day of a scenario
(defn scenario-offset [fc map-data] (:StartDay (first (filter-fc fc map-data))))

;;Returns the ending day of a scenario
(defn scenario-mapend [fc map-data]
  (let [m (first (filter-fc fc map-data))] (+ (:StartDay m) (:Duration m))))

;;Gets the scenario path given the FC and mapfile (file should exist in same directory as mapfile)
(defn forgepath [fc mapfile] (clojure.string/replace mapfile (spork.util.io/fname mapfile) (str "FORGE_" fc ".txt")))

;;Formats forges and syncs timing to map
(defn join-forges [scenarios map-data mapfile & {:keys [phases] :or {phases nil}}]
  (for [forge scenarios
        :let [mapend (scenario-mapend forge map-data)
              forgedata (read-forgefile forge (forgepath forge mapfile))
              phase-info (filter #(= forge (:FC %)) phases)
              min-start (if (pos? (count phase-info))
                          (:Start (first (sort-by #(:Start %) phase-info)))
                          (apply min (map :StartDay forgedata)))
              offset (dec (scenario-offset forge map-data))]]
    (sync-map 
      (map #(assoc % :StartDay (+ offset (:StartDay %))) forgedata) (ff/last-phase forgedata :mapend mapend :offset offset) mapend)))

;;Finds vignettes that are not in map but in cons or not in cons but in map, then writes to log file
(defn write-oos-vignettes [mapfile map-data vignette-data]
  (let [vignettes (filter #(not (scenario? %)) (map :ForceCode map-data))
        oos-file (clojure.string/replace mapfile (spork.util.io/fname mapfile) (str "Out-of-scope-vignettes.txt"))
        map-only (clojure.set/difference (set vignettes) (set (map :ForceCode vignette-data)))
        cons-only (clojure.set/difference (set (map :ForceCode vignette-data)) (set vignettes))
        oos-string (flatten (vector "ForceCode\tReason\n"
                              (map #(str % "\tNot in consolidated\n") map-only)
                              (map #(str % "\tNot in map\n") cons-only)))]
    (spit oos-file (reduce str oos-string))))


;;Reads the mapfile and joins the appropriated data from either the Vignette Consolidated file or FORGE file.
;;For each Vignette in the map, joins timing data from map with quantity information from vignette consolidated file
;;For each Scenario, looks for the correponding Forge file and adds the start day offset and synconized to the map
;; ***If there is not corresponding FORGE file found, will throw error. ONLY include scenarios that have an existing file
;; ***The file name (FORGE_SE-XXXX) has to match what is in the Mapping file
;;Returns a list of maps will the data needed to build the demand record
(defn join-by-map [mapfile vignettefile & {:keys [phases] :or {phases nil}}]
  (let [map-data (read-mapfile mapfile)
        vignette-data (read-vignette vignettefile)
        scenarios (filter scenario? (map :ForceCode map-data))               
        vignettes (filter #(not (scenario? %)) (map :ForceCode map-data))
        joined-vignettes (map #(for [md (filter-fc % map-data)]
                                 (conj md (first (filter-fc % vignette-data)))) vignettes)
        joined-forges (join-forges scenarios map-data mapfile :phases phases)]
    (write-oos-vignettes mapfile map-data vignette-data)
    (filter 
      #(and (> (:Duration %) 0) (not= 0 (:Quantity %)) (not= nil (:Quantity %)))
      (flatten (conj joined-forges joined-vignettes)))))

;;Fields in Demand files
(def demand-fields [:Type :Enabled :Priority :Quantity :DemandIndex :StartDay :Duration :Overlap
                    :SRC :SourceFirst :DemandGroup :Vignette :Operation :Category :Title10_32 
                    :OITitle :Strength])

;;Formatt Vignettes and FORGE maps as Demand data maps
(defn record->demand-record [{:keys [Quantity Duration StartDay SRC ForceCode Title
                                     Strength Vignette Operation Title10_32]}]
 {:Type     "DemandRecord" ;;Type - 0  
  :Enabled   "TRUE" ;;Enabled - 1  
  :Priority  1 ;;Priority - 2  
  :Quantity  Quantity ;;Quantity (of SRCs) - 3  
  :DemandIndex  1 ;;DemandIndex - 4  
  :StartDay StartDay ;;StartDay - 5  
  :Duration Duration ;;Duration - 6  
  :Overlap  45 ;;Overlap - 7   
  :SRC SRC     ;;SRC - 8 
  :SourceFirst "Uniform" ;;SourceFirst - 9  
  :DemandGroup  (or Vignette (if (= "S-" (subs ForceCode 0 2)) ForceCode "Ungrouped")) ;;DemandGroup - 10  
  :Vignette  (or Vignette ForceCode) ;;Vignette - 11  
  :Operation (or Operation ForceCode) ;;Operation - 12  
  :Category  "Rotational" ;;Category - 13  
  :Title10_32 (or Title10_32 "10") ;;Title10_32 - 14 
  :OITitle Title ;;OITitle - 15  
  :Strength Strength}) ;;Strength - 16  

(defn write-shifted-forges [outfile]
  (jio/delete-file outfile true)
  (println "Number of forges shifted: " (count @shifted-forges))
  (doseq [d @shifted-forges]
    (spit outfile (str d "\n") :append true))
  (def shifted-forges (atom [])))

(defn write-dup-recs [outfile]
  (jio/delete-file outfile true)
  (println "Number of duplicate records: " (count @dup-recs))
  (doseq [d @dup-recs]
    (spit outfile (str d "\n") :append true))
  (def dup-recs (atom [])))

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
        phases (when (spork.util.io/fexists? (str root "Phases.txt"))
                 (into [] (spork.util.table/tabdelimited->records (str root "Phases.txt") 
                            :schema {:FC :text :Phase :text :Start :number :Duration :number})))
        outfile      (str root (spork.util.io/fname root) "_DEMAND.txt")
        duplicates-file (str root "Duplicate-records.txt")
        adjusted-file (str root "Adjusted-records.txt")
        data (join-by-map mapfile vignettefile :phases phases)]
    (write-shifted-forges adjusted-file)
    (write-dup-recs duplicates-file)
    (-> (->> (map record->demand-record data)
             (sort-by (juxt :Vignette :SRC :StartDay)))     
        (spork.util.stream/records->file outfile :field-order demand-fields))
    (flatten (vector mapfile vignettefile forgefiles))))

