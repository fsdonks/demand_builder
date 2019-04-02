(ns demand_builder.m4plugin
  (:require [spork.util [io :as io] [table :as tbl]]
            [spork.util.excel [core :as ex] [docjure :as doc]]
            [clojure.java [io :as jio]]
            [demand_builder.formatter :as formatter]))

;;Name of outputdir file will be created in (not full path)
(def outputdir "Outputs")

;;Reads input file. Input files should have the fields Path, Type, ForceCode,
;;and Sheetname Path is the full filepath of the file to be included Type is the
;;file type; this can be either MAP, CONSOLIDATED, or FORGE ForceCode is the
;;force code that is used in the MAP file. Only FORGE files need this field, it
;;can be empty for MAP and CONSOLIDATED types
(defn read-input-file
  [filename & {:keys [schema]
               :or {schema {:Path :text
                            :Type #(clojure.string/upper-case %)
                            :ForceCode :text
                            :Sheetname :text}}}]
  (as-> filename it
    (spork.util.table/tabdelimited->records it :schema schema)
    (into [] it)))

;;Makes new file with content from old file and removes old file
(defn rename-file [old-file new-file]
  (io/fcopy (jio/file old-file) (jio/file new-file))
  (io/delete-file-recursively old-file))

;;Need a way to get the force code from the forge file
;;Gets the ForceCode for the forgefile from the input map field
(defn forge-filename->fc [forgefile input-map]
  (:ForceCode (first (filter #(= forgefile (:Path %)) input-map))))

;;map of seq of rows of data in sheet with sheetname as key and data as val,
;;where p is file path
(defn sheet-rows [p & {:keys [sheets]}]
  (let [wb (ex/xlsx->wb p)]
    (->> (for [s  (doc/sheet-seq wb)
               :when (sheets (doc/sheet-name s))]
           [(doc/sheet-name s) s])
         (into {}))))

;;gets the nth row of from excel file in sheet sheet-name
(defn get-nth-row [xlsx sheet-name n]
  (-> xlsx
      (sheet-rows :sheets #{sheet-name})
      (get sheet-name)
      ex/contiguous-rows
      (nth n)
      ex/row->vec))

;;Option to use non-tabular SRC_by_Day sheet from FORGE in event more specific
;;phase timing is needed for corner cases
(defn forge->non-tab [forgefile dir sheetname scenario]
   (let [phases (get-nth-row forgefile sheetname 0)
         data (get (ex/xlsx->tables forgefile :sheetnames [sheetname]
                                    :options {:default {:skip 1}}) sheetname)
         fields (:fields data)
         outfile (str (io/as-directory dir) scenario ".txt")
         line->tsv (fn [line] (str (apply str (for [v line] (str v "\t"))) "\n"))]
    (when (io/fexists? outfile)
      (clojure.java.io/delete-file outfile))
    (spit outfile (line->tsv phases) :append true)
    (spit outfile (line->tsv fields) :append true)
    (doseq [line (for [i (range (count (first (:columns data))))]
                   (map #(nth % i) (:columns data)))]
      (spit outfile (line->tsv line) :append true))))

;;Reads FORGE data from either the Unit_Node_Detail sheet or SRC_By_Day sheet
(defn forgexlsx->tsv [forgefile dir input-map]
  (let [p (first (filter #(= forgefile (:Path %)) input-map))]
    (if (= (:Sheetname p) "Unit_Node_Detail")
      (ex/xlsx->tabdelimited forgefile :sheetnames [(:Sheetname p)])
      (forge->non-tab forgefile dir (:Sheetname p) (:ForceCode p)))
    (rename-file (str dir (:ForceCode p) ".txt")
                 (str (io/as-directory (str dir outputdir)) "FORGE_"
                      (forge-filename->fc forgefile input-map) ".txt"))))

;;Reads the first line of a tab delimited text file
(defn read-header-txt [file]
  (with-open [r (clojure.java.io/reader file)]
    (clojure.string/split (first (line-seq r)) #"\t")))

;;Map to replace header with expected field name
(def header-map 
  {"Event Code" "ForceCode"
   "EventCode" "ForceCode"
   "Force List Code (ID)" "ForceCode"
   "Start" "StartDay"
   "SRC" "SRC"
   "SRC TITLE" "Title"
   "SRC TITLE`" "Title"
   "STR" "Strength"
   "QTY" "Quantity"
   "Title 10_32" "Title10_32"})
  
;;Uses header-map to replace the header with expected column names
(defn resolve-header [header]
  (for [h header :let [replacement (get header-map h)]]
    (if replacement replacement h)))

;;Will replace header of text file created from exel file with expected names
;;for columns (using header map) To add an additional case where names may be
;;different, just add the given column name to header-map with key of expected
;;value
(defn fix-header [file]
  (let [header (read-header-txt file)
        fixed-header (resolve-header header)
        newfile (str file "-temp")
        new-header-line (str (apply str (map #(str % "\t") fixed-header)) "\n")]
    (do
      (spit newfile new-header-line)
      (with-open [r (clojure.java.io/reader file)]
        (let [lines (drop 1 (line-seq r))]
          (doseq [line lines]
            (spit newfile (str line "\n") :append true))))
      (rename-file newfile file))))


;;Returns filepath of MAP file (only takes first one if multiple)
(defn find-map-file [input-map]
  (:Path (first (filter #(= "MAP" (:Type %)) input-map))))

(defn mapfile->table [mapfile sheetname]
  (let [data (get (ex/xlsx->tables mapfile :sheetnames [sheetname]) sheetname)
        duration-index (get (zipmap (:fields data)
                                    (range (count (:fields data)))) "Duration")]
    (tbl/make-table (:fields data)
                    (update-in (:columns data) [duration-index]
                               #(vec (map (fn [x] (if (not x) -1 x)) %))))))

;;Reads mapfile, replaces missing durations with -1 (will be updated in later
;;formatter functions)
(defn read-mapfile [mapfile sheetname]
  (into [] (-> (mapfile->table mapfile sheetname)
               (tbl/table->tabdelimited)
               (tbl/tabdelimited->records))))

;;Converts excel file to txt and resolves headers
(defn excel->txt-fixed-headers [exfile sheetname outfile]
  (io/hock outfile
    (tbl/table->tabdelimited
      (tbl/make-table (resolve-header (get-nth-row exfile sheetname 0)) 
                      (:columns (get (ex/xlsx->tables
                                      exfile :sheetnames [sheetname]) sheetname))))))

;;List the sheet names in an excel file
(defn list-sheets [exfile]
  (map doc/sheet-name (doc/sheet-seq (doc/load-workbook exfile))))

;;Formatts and moves files into correct location to be able to run demand builder from root
(defn setup-dir [in-map root]
  (let [inputs (io/as-directory (str root outputdir))
        find-file (fn [type] (filter #(= type (:Type %)) in-map))
        vmap (first (find-file "MAP"))
        vcons (first (find-file "CONSOLIDATED"))
        forges (map :Path (find-file "FORGE"))
        new-map (str (io/as-directory (str root outputdir)) (:Sheetname vmap) ".txt")
        new-con (str (io/as-directory (str root outputdir)) (:Sheetname vcons) ".txt")
        _ (io/make-folders! inputs)
        exl->txt (fn [file] (let [sheet (first (list-sheets (:Path file)))]
                              (excel->txt-fixed-headers (:Path file) sheet (str (io/as-directory (str root outputdir)) sheet ".txt"))))
        _ (doall (pmap #(exl->txt %) [vmap vcons]))
        _ (doall (pmap #(forgexlsx->tsv % (io/as-directory root) in-map) forges))]
    (doall (pmap #(rename-file (first %) (clojure.string/replace (first %) (io/fname (first %)) (str (second %) (io/fname (first %)))))
             [[new-map "MAP_"] [new-con "CONSOLIDATED_"]]))))

;;Converts the excel workbooks to tsv and creates the required path structure and file names
(defn setup-directory [input-file]
  (let [root (io/as-directory (clojure.string/replace input-file (io/fname input-file) ""))
        in-map (read-input-file input-file)]
    (setup-dir in-map root)))

;;Builds demand file by formatting inputs according to the input-file
(defn inputfile->demand [input-file]
  (let [_ (setup-directory input-file)
        root (io/as-directory (str (clojure.string/replace input-file (io/fname input-file) "") outputdir))]
    (formatter/root->demandfile root)))

;;returns the sheet object of the excel file with sheet sheetname
(defn get-sheet-by-name [exfile sheetname]
  (first (filter #(= sheetname (doc/sheet-name %)) (doc/sheet-seq (doc/load-workbook exfile)))))

;;reads the sheet data into table from excel file using sheet sheetname
(defn read-sheet [exfile sheetname]
  (ex/sheet->table (get-sheet-by-name exfile sheetname)))

(def detail-schema {:Day :float
                    :SRC :text
                    (keyword "UIN Quantity") :float
                    (keyword "Time Period Begin Day") :float
                    (keyword "Time Period Days") :float
                    :Subphase :text})

;;Reads an excel sheet into a record map
(defn sheet->records [exfile sheetname]
  (into [] (-> (ex/sheet->table (get-sheet-by-name exfile sheetname))
               (tbl/table->tabdelimited)
               (tbl/tabdelimited->records :schema detail-schema))))

;;reads the fields of an exfile sheet
(defn read-header [exfile sheetname]
  (try
    (:fields (ex/sheet->table (get-sheet-by-name exfile sheetname)))
    (catch java.lang.IllegalArgumentException iae 
      (throw (ex-info
               (str "Forumla evaluation resulted in error in \nfile: " exfile " \nsheet: " sheetname "\n Fix Excel file before running.") {:input 42})))
    (catch java.lang.RuntimeException nyi
      (if (= (str nyi) "java.lang.RuntimeException: not implemented yet")
        (throw (ex-info (str "Remote VLOOKUPs not supported. " "Resolve VLOOKUPs in \nfile: " exfile "\nsheet: " sheetname) {:input 42}))
        (throw (ex-info (str nyi) {:input 42}))))))

;;Expected headers for Unit_Node_Detail, Vignette consolidated, and Vignette
;;mapping files
(def und-header
  ["UIN Quantity" "Time Period Begin Day" "Time Period Days" "Subphase" "SRC
  Strength" "SRC" "Title"])
(def vcons-header
  ["ForceCode" "SRC2" "SRC" "Title" "Strength" "Quantity" "Title10_32"])
(def map-header
  ["ForceCode" "TAA 20-24 ISC Scenarios and Vignettes" "BCT Original" "BCT
  New" "StartDay" "Duration" "BCT Quantity"])

;;Map of headers with key as the file type
(def headers {und-header "FORGE" vcons-header "CONSOLIDATED" map-header "MAP"})

;;Counts the number of fields in comp that exist in some each header
(defn header-count [header comp]
  (let [hs (set header)]
    (count (filter #(contains? hs %) comp))))

;;Uses file header or sheet names to try to determine the file type [FORGE, MAP,
;;or CONSOLIDATED]
(defn most-likely-file [exfile]
  (let [sheets (set (list-sheets exfile))]
    (if (or (contains? sheets "Unit_Node_Detail") (contains? sheets "SRC_By_Day"))
      "FORGE"
      (let [h (read-header exfile (first sheets))
            counts (zipmap (map #(header-count % h) (keys headers)) (vals headers))]
        (get counts (apply max (keys counts)))))))

;;List all .xlsx files located in root
(defn list-excel-files [root]
  (filter #(re-find #".xlsx" %) (map str (io/list-files (io/file root)))))

;;List all files of file type in root [FORGE, MAP, or CONSOLIDATED]
(defn find-file-type [root type]
  (filter #(= type (most-likely-file %)) (list-excel-files root)))

;;Reads the forgefile and determines the forges start, end, and duration times
(defn forge-time [forgefile]
  (let [s (first (filter #(= "Unit_Node_Detail" %) (list-sheets forgefile)))
        r (sheet->records forgefile s)
        min-start (apply min (map (keyword "Time Period Begin Day") r))
        max-end (apply max (map #(+ (get % (keyword "Time Period Begin Day"))
                                    (get % (keyword "Time Period Days"))) r))]
    {:start min-start :end max-end :duration (- max-end min-start)}))

;;Reads the map files and determines the start and duration of each event
(defn map->scenario-times [mapfile]
  (let [r (filter #(= "SE-" (apply str (take 3 (str (:ForceCode %))))) 
            (read-mapfile mapfile (first (list-sheets mapfile))))]
    (for [i r]
      {:fc (:ForceCode i) :start (:StartDay i) :duration (:Duration i)})))

;;Numeric value to used to try to determine which force code a FORGE file is
;;associated with The larger the number, the less likily the FORGE corresponds
;;to that fc Checks the difference between the ForceCode and file name
(defn fc-weight [title fc]
  (let [st (set title) sfc (set fc)]
    (hash-map fc (count (clojure.set/union (clojure.set/difference st sfc)
                                           (clojure.set/difference sfc st))))))

;;Weight the liklihood of each forge file having fc based on duration listed in
;; map (difference from forge) and percentage of filename matching
(defn get-fc-weight [forgefile fc forge-time map-times]
  (let [mt (first (filter #(= fc (:fc %)) map-times))
        dm (- (:duration mt) (:duration forge-time))]
    (+ (get (fc-weight forgefile fc) fc) (* dm dm))))

(defn closest-fc [forgefile forge-time fcs map-times]
  (let [m (zipmap (map #(get-fc-weight forgefile % forge-time map-times) fcs) fcs)]
    {forgefile (get m (apply min (keys m)))}))

(defn forges->fc [forges forge-times map-times fcs  r]
  (if (pos? (count forges))
    (let [ff (first forges)
          fc (closest-fc ff (get forge-times ff) fcs map-times)]
      (forges->fc
        (drop 1 forges)
        (dissoc forge-times ff)
        (filter #(not= (get fc ff) (str (:fc %))) map-times)
        (filter #(not= (get fc ff) %) fcs)
        (into r fc)))
    r))

;;Get Scenario ForceCodes from MAP
(defn map->scenario-fcs [mapfile]
  (let [r (filter #(= "SE-" (apply str (take 3 (str (:ForceCode %))))) 
            (read-mapfile mapfile (first (list-sheets mapfile))))]
    (map :ForceCode r)))

;;For each forge in root, tries to match the most likely ForceCode form Map
;;using filename and duration data
;;Randomly match, don't read FORGE an extra time - too slow
(defn match-forge-fc [root]
  (let [forges (find-file-type root "FORGE")
        fcs (map->scenario-fcs (first (find-file-type root "MAP")))]
    (zipmap forges fcs)))


;;Attempts to determine which sheet to use from excel file
;;Throws exception asking to user input if can't be determined from metadata
;;Returns string of sheetname
(defn get-probable-sheet [file]
  (let [sheets (list-sheets file)]
    (cond 
      (= 1 (count sheets)) (first sheets)
      ;;ideally, I think we want to use Unit_Node_Detail first, but
      ;;that's not working right now.
      (some #{"SRC_By_Day"} sheets) "SRC_By_Day"
      (some #{"Unit_Node_Detail"} sheets) "Unit_Node_Detail"

      ;Either the file is FORGE and attempts to use Unit_Node_Detail, and uses
      ;SRC_By_Day only when it does not exist
      ;;If the file is not a FORGE file and has more than one sheet, throw error
      ;;asking for user input
      true (throw (ex-info "\tCould not automatically determine which sheet to user.\n
                    User unput require for generating input map file." {:input 42})))))

;;Creates file with files needed for demand, their path, file type,
;;force-code (when FORGE, otherwise empty), and sheetname
(defn root->inputmap [root]
  (let [output (str (io/as-directory root) "input-map.txt")
        files (list-excel-files root)
        ;;Random matching - let the user fix file mappings in gui
        forge-fcs (match-forge-fc root)] 
    (when (io/fexists? output)
      (clojure.java.io/delete-file output))
    (doseq [i (concat ["Path\tType\tForceCode\tSheetname\n"]
                (for [f files :let [type (most-likely-file f)]]
                  (str f "\t" type "\t" (if (= "FORGE" type) (get forge-fcs f) "none")
                         "\t" (get-probable-sheet f) "\n")))]
      (spit output i :append true))
    output))

;;Gets file paths and meta data from inputmap, converts inputs to txt files,
;;then runs demand builder formatter
(defn root->demand-file [root]
  (inputfile->demand (root->inputmap root)))
