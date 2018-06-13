(ns demand_builder.formatter
    (:require [spork.util table parsing io]
              [demand_builder [forgeformatter :as ff]]))

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

;;Takes a list of maps (of FORGE data), the last phase for the FORGE scenario, and the final day for the scenario from the map (Start + Duration from map)
;;For the last period of each record, if the phase matches last phase, will change the duration to match the ending time specified by the map
;; **This can either increase or decrease the initial duration
;;Returns a list of maps (of FORGE data) that has been syncronized to end on the same day as what is listed in the map file.
(defn sync-map [forgerecords lastphase mapend]
  (let [splitrecs (partition-by :SRC (filter #(= lastphase (:Operation %)) forgerecords))
        lastrecs (map #(last (sort-by :StartDay %)) splitrecs)
        adjusted (map #(assoc % :Duration (+ (:Duration %) (- mapend (:StartDay %) (:Duration %)))) lastrecs)]
    (filter #(pos? (:Duration %))
      (flatten (conj adjusted (into [] (clojure.set/difference (set forgerecords) (set lastrecs))))))))

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
                                              (ff/forge->records forgefile)
                                              (catch Exception e (throw (Exception. (str "File not found for FOREGE_" forge "\n" (.getMessage e))))))
                                  
                                  ;phases (:phases (read-forge forgefile))
                                  offset (scenario-offset forge map-data)]]
                        (sync-map (map #(assoc % :StartDay (+ offset (:StartDay %))) forgedata) (ff/last-phase forgedata) mapend))]
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

