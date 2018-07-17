(ns demand_builder.m4plugin
  (:require [spork.util.excel [core :as ex]]
            [spork.util [io :as io]]
            [spork.util [table :as tbl]]
            [spork.util [io :as io] [table :as tbl]]
            [spork.util.excel [core :refer :all] [docjure :refer :all]]
            [clojure.java [io :as jio]]
            [demand_builder.formatter :as formatter]))

;;Name of outputdir file will be created in (not full path)
(def outputdir "Outputs")

;;Reads input file.
;;Input files should have the fields Path, Type, ForceCode, and Sheetname
;;Path is the full filepath of the file to be included
;;Type is the file type; this can be either MAP, CONSOLIDATED, or FORGE
;;ForceCode is the force code that is used in the MAP file. Only FORGE files need this field, it can be empty for MAP and CONSOLIDATED types
(defn read-input-file [filename & {:keys [schema] :or {schema {:Path :text :Type #(clojure.string/upper-case %) :ForceCode :text :Sheetname :text}}}]
  (as-> filename it (spork.util.table/tabdelimited->records it :schema schema) (into [] it)))

;;Makes new file with content from old file and removes old file
(defn rename-file [old-file new-file]
  (io/fcopy (jio/file old-file) (jio/file new-file))
  (io/delete-file-recursively old-file))

;;Need a way to get the force code from the forge file
;;Gets the ForceCode for the forgefile from the input map field
(defn forge-filename->fc [forgefile input-map]
  (:ForceCode (first (filter #(= forgefile (:Path %)) input-map))))


(in-ns 'spork.util.excel.core)
;; ========== DON'T REUSE ==========
(defn tabular-region
  ([sheet] (tabular-region sheet +default-options+))
  ([sheet options]
   (let [{:keys [skip read-cell ignore-dates?]
          :or   {skip          0
                 read-cell     doc/read-cell
                 ignore-dates? true}} options
         read-cell  (as-cell-reader read-cell)
         fields     (truncate-row (row->vec (nth (contiguous-rows sheet) skip)))
         fieldcount (count fields)
         pooled     (s/->string-pool 100 1000)
         read-cell-pooled (fn [cl]
                            (let [res (read-cell cl)]
                              (if (string? res) (pooled res) res)))]
     (binding [doc/*date-checking* (not ignore-dates?)]
       (->> (contiguous-rows sheet)
            ;;(drop skip) - need to keep the skiped rows, they still contain data for formatting
            ;;In this specific case, we need the data from the first row,
            ;;but we start counting the tabular region form the second
            ;;The first row is the phase timinings (non-tabular)
            ;;The second row is the real header
            (map (fn [r]
                   (let [r (row->vec r nil read-cell-pooled)
                         rcount (count r)]
                     (cond (= rcount fieldcount) r
                       (> rcount fieldcount) (subvec r 0 fieldcount)
                       (< rcount fieldcount) (into r (take (- fieldcount rcount) 
                                                       (repeat nil)))))))
            (take-while (complement empty-row?))))))) ;;we infer a blank row as the end of the table.
(in-ns 'demand_builder.m4plugin)

;;Option to use non-tabular SRC_by_Day sheet from FORGE in event more specific phase timing is needed for corner cases
(defn forge->non-tab [forgefile dir sheetname]
  (ex/xlsx->tabdelimited forgefile :rootdir dir :sheetnames [sheetname] :options {:default {:skip 1}}))

(defn forgexlsx->tsv [forgefile dir input-map]
  (let [p (first (filter #(= forgefile (:Path %)) input-map))]
    (if (= (:Sheetname p) "Unit_Node_Detail")
      (ex/xlsx->tabdelimited forgefile :rootdir dir :sheetnames [(:Sheetname p)])
      (forge->non-tab forgefile dir (:Sheetname p)))
    (rename-file (str dir (:Sheetname p) ".txt") (str dir "FORGE_" (forge-filename->fc forgefile input-map) ".txt"))))

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

;;Will replace header of text file created from exel file with expected names for columns (using header map)
;;To add an additional case where names may be different, just add the given column name to header-map with key of expected value
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

(defn setup-dir [in-map root]
  (let [inputs (io/as-directory (str root outputdir))
        find-file (fn [type] (filter #(= type (:Type %)) in-map))
        vmap (first (find-file "MAP"))
        vcons (first (find-file "CONSOLIDATED"))
        forges (map :Path (find-file "FORGE"))
        _ (io/make-folders! inputs)
        _ (ex/xlsx->tabdelimited (:Path vmap) :rootdir inputs)
        _ (ex/xlsx->tabdelimited (:Path vcons) :rootdir inputs)
        _ (doseq [f forges] (forgexlsx->tsv f (io/as-directory (str root outputdir)) in-map))
        new-map (str (io/as-directory (str root outputdir)) (:Sheetname vmap) ".txt")
        new-con (str (io/as-directory (str root outputdir)) (:Sheetname vcons) ".txt")]
    (fix-header new-map)
    (fix-header new-con)
    (rename-file new-map (clojure.string/replace new-map (io/fname new-map) (str "MAP_" (io/fname new-map))))
    (rename-file new-con (clojure.string/replace new-con (io/fname new-con) (str "CONSOLIDATED_" (io/fname new-con))))))

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

(defn list-sheets [exfile]
  (map sheet-name (sheet-seq (load-workbook exfile))))

(defn get-sheet-by-name [exfile sheetname]
  (first (filter #(= sheetname (sheet-name %)) (sheet-seq (load-workbook exfile)))))

(defn read-sheet [exfile sheetname & {:keys [skip] :or {skip nil}}]
  (let [sheet (get-sheet-by-name exfile sheetname)
        opts (if skip (assoc +default-options+ :skip skip) +default-options+)
        data (sheet->table sheet opts)
        header (:fields data)
        cols (:columns data)]
    (for [i (range (count header))]
      (zipmap (map keyword header) (map #(nth % i) cols)))))

(defn read-sheet [exfile sheetname]
  (sheet->table (get-sheet-by-name exfile sheetname)))

(defn file->record [exfile sheetname]
 (into [] (tbl/tabdelimited->records (tbl/table->tabdelimited (read-sheet exfile sheetname)))))

(defn sheet->records [exfile sheetname]
  (into [] (-> (sheet->table (get-sheet-by-name exfile sheetname))
               (tbl/table->tabdelimited)
               (tbl/tabdelimited->records))))

(defn read-header [exfile sheetname]
  (:fields (sheet->table (get-sheet-by-name exfile sheetname))))

(def und-header   ["UIN Quantity" "Time Period Begin Day" "Time Period Days" "Subphase" "SRC Strength" "SRC" "Title"])
(def vcons-header ["ForceCode" "SRC2" "SRC" "Title" "Strength" "Quantity" "Title10_32"])
(def map-header   ["ForceCode" "TAA 20-24 ISC Scenarios and Vignettes" "BCT Original" "BCT New" "StartDay" "Duration" "BCT Quantity"])

(def headers {und-header "FORGE" vcons-header "CONSOLIDATED" map-header "MAP"})

(defn header-count [header comp]
  (count (filter #(contains? (set header) %) comp)))

(defn most-likely-file [exfile]
  (let [sheets (set (list-sheets exfile))]
    (if (or (contains? sheets "Unit_Node_Detail") (contains? sheets "SRC_By_Day"))
      "FORGE"
      (let [h (read-header exfile (first sheets))
            counts (zipmap (map #(header-count % h) (keys headers)) (vals headers))]
        (get counts (apply max (keys counts)))))))
        
(defn possilbe-scenarios [mapfile sheet]
  (filter #(= "SE-" (apply str (take 3 (str %)))) (apply concat (:columns (read-sheet mapfile sheet)))))

(defn list-excel-files [root]
  (filter #(re-find #".xlsx" %) (map str (.listFiles (java.io.File. root)))))

(defn find-file-type [root type]
  (filter #(= type (most-likely-file %)) (list-excel-files root)))

(defn forge-time [forgefile]
  (let [s (first (filter #(= "Unit_Node_Detail" %) (list-sheets ff)))
        r (sheet->records forgefile s)
        min-start (apply min (map (keyword "Time Period Begin Day") r))
        max-end (apply max (map #(+ (get % (keyword "Time Period Begin Day")) (get % (keyword "Time Period Days"))) r))]
    {:start min-start :end max-end :duration (- max-end min-start)}))
    
(defn map->scenario-times [mapfile]
  (let [r (filter #(= "SE-" (apply str (take 3 (str (:ForceCode %))))) 
            (sheet->records mapfile (first (list-sheets mapfile))))]
    (for [i r]
      {:fc (:ForceCode i) :start (:StartDay i) :duration (:Duration i)})))

(defn fc-weight [title fc]
  (let [st (set title) sfc (set fc)]
    (hash-map fc (count (clojure.set/union (clojure.set/difference st sfc) (clojure.set/difference sfc st))))))

;;Weight the liklihood of each forge file having fc 
;; based on duration listed in map (difference from forge) and percentage of filename matching
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

(defn match-forge-fc [root]
  (let [mapfile (first (find-file-type root "MAP"))
        forges (find-file-type root "FORGE")
        map-times (map->scenario-times mapfile)
        forge-times (zipmap forges (map forge-time forges))
        fcs (map :fc map-times)
        mapped-fcs (forges->fc forges forge-times map-times fcs [])]
    (zipmap (map first mapped-fcs) (map second mapped-fcs))))

(defn get-probable-sheet [file]
  (let [sheets (list-sheets file)]
    (if (= 1 (count sheets)) ;;Mapping and Consolidated files should only have a single sheet
      (first sheets)
      (if (first (filter #(= "Unit_Node_Detail" %) sheets))
        "Unit_Node_Detail"
        (if (first (filter #(= "SRC_By_Day" %) sheets))
          "SRC_By_Day"
          ;;Either the file is FORGE and attempts to use Unit_Node_Detail, and uses SRC_By_Day only when it does not exist
          ;;If the file is not a FORGE file and has more than one sheet, throw error asking for user input
          (throw (ex-info 
                   "\tCould not automatically determine which sheet to use.\n
                   User input required for generating input map file." {:input 42})))))))

(defn root->inputmap [root]
  (let [output (str (io/as-directory root) "input-map.txt")
        files (list-excel-files root)
        forge-fcs (match-forge-fc root)
        lines (concat ["Path\tType\tForceCode\tSheetname\n"]
                (for [f files :let [type (most-likely-file f)]]
                  (str f "\t" type "\t" (if (= "FORGE" type) (get forge-fcs f) "none") "\t" (get-probable-sheet f) "\n")))]
    (.delete (java.io.File. output))
    (doseq [i lines]
      (spit output i :append true))
    output))
     
(defn root->demand-file [root]
  (inputfile->demand (root->inputmap root)))

(def ff "C:\\Users\\michael.m.pavlak.civ\\Desktop\\complexest\\Input\\Excel\\Scenario-99-TAA-20-24.xlsx")
(def mf "C:\\Users\\michael.m.pavlak.civ\\Desktop\\complexest\\Input\\Excel\\Vignette Mapping - TAA 20-24.xlsx")
(def vf "C:\\Users\\michael.m.pavlak.civ\\Desktop\\complexest\\Input\\Excel\\Vignette Consolidated - TAA 20-24.xlsx")
(def root "C:\\Users\\michael.m.pavlak.civ\\Desktop\\complexest\\Input\\Excel\\")


