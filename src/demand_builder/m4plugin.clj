(ns demand_builder.m4plugin
  (:require [spork.util.excel [core :as ex]]
            [spork.util [io :as io]]
            [spork.util [table :as tbl]]
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

;;Overrode some of the functions in spork.excel
;;These are specific *ONLY* to the SRC_by_Day sheet of FORGE
;;Do not reuse these functions for anything else
; ==================================
;; ========== DON'T REUSE ==========
(in-ns 'spork.util.excel.core)

;; ========== DON'T REUSE ==========
(defn tabular-region2
  [sheet & {:keys [i] :or {i 0}}]
  (let [fields (truncate-row (row->vec (nth (contiguous-rows sheet) i)))
        fieldcount (count fields)
        pooled     (s/->string-pool 100 1000)
        read-cell-pooled (fn [cl]
                           (let [res (clojure.string/replace (read-cell cl) #"[\n]" " ")]
                             (if (string? res) (pooled res) res)))]
    (->> (contiguous-rows sheet) 
         (map (fn [r]
                (let [r (row->vec r nil read-cell-pooled)
                      rcount (count r)]
                  (cond (= rcount fieldcount) r
                    (> rcount fieldcount) (subvec r 0 fieldcount)
                    (< rcount fieldcount) (into r (take (- fieldcount rcount) 
                                                    (repeat nil)))))))
         (take-while (complement empty-row?)))))

;; ========== DON'T REUSE ==========
(defn sheet->table2
  [sheet & {:keys [i] :or {i 0}}] 
  (let [rows    (tabular-region2 sheet :i i)]
    (when-let [fields  (first rows)]
      (if-let [records (vec (rest rows))]
        (tbl/make-table fields (v/transpose  records))
        (tbl/make-table fields)))))

;; ========== DON'T REUSE ==========
(defn wb->tables2
  [wb & {:keys [sheetnames i] :or {sheetnames :all i 0}}]
  (let [sheets  (sheet-seq wb)]
    (->> (if (= sheetnames :all) sheets
           (let [names (set (map lower-case sheetnames))]
             (filter #(contains? names ((comp lower-case sheet-name) %))
               sheets)))
         (map (fn [s] (do (println (sheet-name s))
                        [(sheet-name s) (sheet->table2 s :i i)])))
         (into {}))))

;; ========== DON'T REUSE ==========
(defn xlsx->tabdelimited2
  [wbpath & {:keys [rootdir sheetnames i] 
             :or {sheetnames :all rootdir (workbook-dir wbpath) i 0}}]
  (let [tmap (wb->tables2 (load-workbook wbpath) :sheetnames sheetnames :i i)]
    (doseq [[nm t] (seq tmap)]
      (let [textpath (io/relative-path rootdir [(str nm ".txt")])]
        (io/hock textpath (tbl/table->tabdelimited t))))))

(in-ns 'demand_builder.m4plugin)
;; ========== DON'T REUSE ==========
;; =================================

;;Option to use non-tabular SRC_by_Day sheet from FORGE in event more specific phase timing is needed for corner cases
(defn forge->non-tab [forgefile dir sheetname]
  (ex/xlsx->tabdelimited2 forgefile :rootdir dir :sheetnames [sheetname] :i 1))

(defn forgexlsx->tsv [forgefile dir input-map]
  (let [p (first (filter #(= forgefile (:Path %)) input-map))]
    (if (= (:Sheetname p) "Unit_Node_Detail")
      (ex/xlsx->tabdelimited forgefile :rootdir dir :sheetnames [(:Sheetname p)])
      (forge->non-tab forgefile dir (:Sheetname p)))
    (rename-file (str dir (:Sheetname p) ".txt") (str dir "FORGE_" (forge-filename->fc forgefile input-map) ".txt"))))

;;Reads the first line of a tab delimited text file
(defn read-header [file]
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
  (let [header (read-header file)
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

