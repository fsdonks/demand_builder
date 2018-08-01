;; ============================================================================
(ns demand_builder.core
  (:gen-class)
  (:require [clojure.java [io :as io]]
            [spork.util [io :refer [list-files fpath fname fext write! writeln!]]]
            [demand_builder [chart :as c]])
  (:import [java.io File FileNotFoundException]
           [javax.swing JFrame JFileChooser JTextArea JPanel JLabel]))


(set! *warn-on-reflection* true)
;; ============================================================================
;; ============================================================================
;A close operation of 3 will kill my repl, but we might want it for the uberjar.
(def ^:dynamic *closeon* 2)

(defn ->frame [& args] ;;Window for error message, first args is Title, second args is message
  (let [f (JFrame.) p (JPanel.) ta (JTextArea.)]
    (.setSize f 500 500) (.setDefaultCloseOperation f *closeon*)
    (.setRows ta 100) (.setSize ta 475 475) (.setLineWrap ta true)
    (.setTitle f (first args)) (.setText ta (second args))
    (.add p ta) (.add f p) (.setVisible f true) f))

;;Returns num from string without throwing errors 
(defn read-num [string]
  (if string
    (let [num (fn [string] (apply str (map #(re-matches #"[\d.]*" %) (map str string))))
          n (clojure.string/split (num string) #"[.]")  t (take 2 n) b (drop 2 n)
          d (read-string (str (first t) "." (second t) (apply str b)))]
      (if (zero? (- d (int d))) (int d) d))
    0))

;; ============================================================================
;; ===== FUNCTION FOR GENERAL IO ==============================================
;; ============================================================================

;; Reads lines from tsv (tab seperated value) file
;; Returns a vector of data from each line  
(defn csv->lines [filename] ;; was changed from csv format to tab delimited text file
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce into ;;can't be lazy, has to read data while file is open, then closes file
            ;; is needs to be lazy, all processing that requires csv lines needs to be wrapped inside this function 
            (for [line (line-seq rdr) :let [data (clojure.string/split line #"\t")]] ;;split with delimiter "\t" as argument
              (conj [] data)))))

;;Given a function to format key, creats a map from lines with function called on first value in line
;; Helper function used to format data from file
(defn lines->map [lines fn-key]           ;; first value is always used in this function
  (reduce conj (for [val lines] {(fn-key (first val)) val})))

;; Reads the data from a tsv text file and creates a map using optional fn to format key
;; Helper function used to format data from file
(defn file->map [filename & fn-key]
  (lines->map (csv->lines filename) (if fn-key (first fn-key) identity)))

;; If first value of val not unique, use multiple vals as keys
;; Helper function used to format data from file
(defn lines->vecmap [lines fn1 fn2] ;;in fn1 and fn2, need to specify key or get key from nth value in val
  (reduce conj (for [val lines] {[(fn1 val) (fn2 val)] val})))
;; ex: (nth % 3) (nth % 7) will use the 3rd and 7th attibute in each line to build a vector to use as the map key

;; Reads file and formats map using fn1 and fn2 as functions to format vector for map keys 
;; Helper function used to format data from file
(defn file->vecmap [filename fn1 fn2]
  (lines->vecmap (csv->lines filename) fn1 fn2))

(defn read-header [file] ;;Reads first line of file and maps column name to index number
  (let [h (with-open [r (clojure.java.io/reader file)] (clojure.string/split (first (line-seq r)) (re-pattern "\t")))]
    (zipmap (map #(keyword (.trim (.toLowerCase ^String %))) h) (range (count h)))))
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ===== MAPS FOR FILE FORMATS AND CONSTANTS ==================================
;; ============================================================================

;;Builds index map from ordered header collection
(defn build-index [ks]
  (reduce conj
          (for [r (range (count ks))] (assoc {} (keyword (nth ks r)) r))))

;; Format of headers in MAP, CONSOLIDATED, and FORGE files
;; If changes are made to the format of the input file, 
;; change the vector that is set as keys when defining the file's index map
(def v-map 
  (let [keys ["force-code" "vignette" "bct-old" "bct-new" "time-start"]]
    ;; manually coppied header from example MAP file
    (build-index keys)))

(def v-cons 
  (let [keys ["force-code" "src2" "src" "title" "str" "quantity" "title_10" "non-rot"]]
    ;; manually coppied header from example CONSOLIDATED file
    (build-index keys)))

(def forge
  (let [keys ["src" "title" "str" "branch-code" "branch-label" "service" "time-start"]]
    ;; manually coppied header from example FORGE file
    (build-index keys)))

(def quart-day-mult 91) ;; constant used to convert quarter time periods to days     
;; ============================================================================  
;; ============================================================================





;; ============================================================================
;; ===== FUNCTIONS TO HANDLE PHASE INFORMATION ================================
;; ============================================================================

;; Takes a map and value and 
;; Returns a coll of all keys that have the given value
(defn keys-with-val [m val] 
  (sort (filter #(= val (get m %)) (keys m))))

;; *Assumption: for these functions to work, the first line in a forge file has to be the phase information
;;              and the second line in the forge file has to be the header information
;; Example:
;; line 1: 		FwdStation	PH I	PH IIa		PH IIb			PH IIc
;; line 2: SRC	Title	Strength	Branch Code	Branch Label	Service	"Day 0001   TP1" "Day 0002    TP2" .....

;; In line 1, the white space indicates the duration between phases which can be derived from the corresponding time period
;; In line 2, the time period entries are in the format: "Day 000x[newline]TPx",
;;            where all information for a single period is in one excel cell


;; Notation: fdata and fd are the returned values from (csv->lines FORGE_FILE)
;;           p is the return value from calling phase-header,
;;           m is the return value from calling phase-indx

;; Returns the phase header line 
(defn phase-header [fdata]
  (drop (:time-start forge) (first fdata))) 

;; Returns a map with phase header title as keys and the index value in the header as their value
(defn phase-indx [fdata]
  (let [h (phase-header fdata)]
    (zipmap h (range (count h)))))

;; Used to convert between time period and days
;; Returns a map with time periods [0 - ~50] as keys and the corresponding day as their value
;; Will only work when the *assumption about the header format are true

;; If format does change, replace this function with alternative method of creating the same map 
(defn day-map [fd]
  (let [r (drop-last (map last (take-while #(= \T (ffirst %)) (drop 2 fd))))
        n (map #(Integer. ^String (last (clojure.string/split % #"Day "))) r)] ;; *assumption has to hold here
    (assoc ;; assumes tp1 always starts as day 001
     (zipmap (range 1 (inc (count r))) n) 0 1)))
;; example output {0 1, 1 9, 2 17, 3 25, .... }   

(def day-regex #"Day \d+")

(defn forge-days
  "returns the sequence of day data from the forge data, fd."
  [fd]
  (take-while (fn [r] (re-find day-regex (reduce str r)))  (drop 2 fd)))

;; Given the phase header p, returns a map with phase-name, start, and end value for first phase in p
(defn make-phase [p m fd]
  (let [n (next p)]
    {:phase-name (first p)
     :start (get m (first p))
     :end (if n (get m (first n))
              (inc (count (forge-days fd))))}))

;; Repeately calls make phases until the whole phase header to mapped
(defn make-phases [p m r fd] ;; r is the collection of returned values
  (if (identity p)
    (make-phases (next p) m (conj r (make-phase p m fd)) fd)
    (filter #(not= "" (:phase-name %)) r))) ;;have to make sure there are no empty strings between phases

;; Given a time period and the start and stop times of a time sequence,
;; Returns true when time period is between the start and stop times
(defn in-phase? [tp start stop]
  (if stop (and (>= tp start) (< tp stop)) (>= tp start)))

;; Given a time period and fd, returns the corresponding phase title
(defn get-phase [tp fd]
  (let [phases (make-phases (filter #(not= "" %) (phase-header fd))
                            (phase-indx fd) [] fd)
        t (map #(in-phase? tp (:start %) (:end %)) phases)]
    (:phase-name (nth phases (get (zipmap t (range (count t))) true)))))

;; Returns the phase that is active during the given day  
(defn get-phase-from-day [day fd]
  (get-phase (first (keys-with-val (day-map fd) day )) fd)) 
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ===== HELPER FUNCTIONS FOR FORMATTING TIME DATA ============================
;; ============================================================================

;; Used for formatting time data in MAP and FORGE files  

;; Takes line of data from file and index where time data starts and 
;; removes empty time values from list,
;; and returns map where the indices of non-empty values are keys and their
;; corresponding quantities are values
(defn filter-times [line time-start] 
  (let [times  (zipmap (for [i (range (- (count line) time-start))] i)
                       (drop time-start line))
        nil-times (filter #(= "" (get times %)) (keys times))]
    (apply dissoc times nil-times)))

;; Helper function for spliting data based on gaps in the time sequence
;; Returns the first continously increasing function from coll x
;; x has to be in the form [value, remaining time sequence]
(defn cont-seq [x]
  (let [a (first x) b (last x)]
    (if (nil? (next a))
      [false [] (conj b (first a))]
      (if (= 1 (- (first (next a)) (first a)))
        [(next a) (conj b (first a))]
        [false (next a) (conj b (first a))])))) 

;; Helper function for spliting data based on gaps in the time sequence
;; Uses cont-seq to split coll x by the first continously increasing sequence.  
(defn split-by-seq [x]
  (let [x [x []] q (last (take-while #(first %) (iterate cont-seq x)))]
    [(drop 1 (first q)) (conj (last q) (ffirst q))])) 
;; example useage: (split-by-seq [1 2 3 5 6 7 9 10 11])
;; returns [(5 6 7 9 10 11) (1 2 3)]

;; Helper function for spliting data based on gaps in the time sequence
;; Returns all time sequences in x
;; by recursively calling split-by-seq an empty list is reached
(defn get-seqs [x & seqs]
  (let [split (split-by-seq x) seqs (if seqs (first seqs) [])]
    (if (= () (first split))
      (conj seqs (last split))
      (get-seqs (first split) (conj seqs (last split))))))
;; example useage: (get-seqs [1 2 3 5 6 7 9 10 11])
;; returnts [[1 2 3] [5 6 7] [9 10 11]]

;; Returns a map made from ks, where ks is a subset of the collection of keys of m
(defn sub-map [m ks]
  (zipmap ks (map #(get m %) ks)))

;; Helper function for determining continous sequences; used by ->split
;; Given map m and some val, partitions keys of map based on their values
(defn partition-keys [m val]
  (partition-by #(= val (get m %)) (keys m)))
;; example usage: (partition-keys {1 "A" 2 "A" 3 "B" 4 "C" } "A")
;; returns ((1 2) (3 4))

;; Returns a coll of all unique quanities across all times 
;; Helper function for determining continous sequences
(defn unique-vals [m] ;; m time-index quanity map (result from filter-times)
  (vec (into #{} (map #(get m %) (keys m)))))
;; example useage: (unique-vals (drop (time-start forge) lines))

;; Returns coll of index value maps that are split by values of keys
;; Could possibly change to use iterate instead of non tail-end recursion
(defn ->split [m]
  (let [vals (unique-vals m)]
    (if (= 1 (count vals)) [m]
        (do (let [par (partition-keys m (first (unique-vals m)))]
              (flatten (conj []
                             (->split (sub-map m (first (next par))))
                             (->split (sub-map m (first par))))))))))
;; example useage (->split {1 "A", 20 "A", 33 "A", 4 "B", 5 "B", 6 "C"})
;; returns ({1 "A", 20 "A", 33 "A"} {4 "B", 5 "B"} {6 "C"}) 

;; Returns a coll of key sequences by unique value
;; Note: this does basically the same thing as ->split (can just map get onto map) and works better/faster
(defn unique-val-seqs [m]
  (for [x (unique-vals m)]
    (sort (keys-with-val m x))))
;; example useage (unique-val-seqs  {1 "A", 20 "A", 33 "A", 4 "B", 5 "B", 6 "C"})
;; returns ((1 20 33) (4 5) (6))
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ===== FUNCTIONS USED FOR FORMAT FORGE DATA =================================
;; ============================================================================

;; Returns vals split by p, where p is the result of some function that partitions vals
(defn split-by-p [p vals fd]
  (let [s (map count p) c (for [x (range 1 (inc (count s)))] (apply + (take x s)))]
    (for [x (range (count s))]
      (drop (- (nth c x) (nth s x)) (take (nth c x) vals)))))

;; Given a coll of time sequence keys, returns the coll split by phase  
(defn split-by-phase [vals fd]
  (split-by-p (partition-by identity (map #(get-phase % fd) (sort vals))) vals fd))

;; Given a coll of time sequence keys, return the coll split by value
(defn split-by-val [vals fd]
  (let [p (partition-by identity (map #(get vals %) fd))
        s (map count p)
        c (for [x (range 1 (inc (count s)))] (apply + (take x s)))]
    (for [x (range (count s))]
      (drop (- (nth c x) (nth s x)) (take (nth c x) fd)))))

;; Given times and forge data, split the keys of times by phase then by value
(defn split-all [times fd]
  (apply concat (for [q (get-seqs (sort (keys times)))]
                  (apply concat (for [qq (split-by-phase q fd)]
                                  (split-by-val times qq))))))

;; Builds time data for forge given time map and forge data
;; Returns a collection of maps with keys :start, :duration, and :quantity
;; Fix error in calculation of duration that resulting in duration of zero for last Forge time period of size one
;; If the last and first period are the same, then the duration is 8 rather than 0. 
(defn ->forge-quantity-at-time [times fd]
  (let [dm (day-map fd)]
    (for [x (split-all times fd) :let [f (first x)
                                       l (if (< (last x) (dec (last (sort (keys dm))))) (inc (last x)) (last x))
                                       dif (- (get dm l) (get dm f))]]
      {:start (get dm (first x)) :duration (if (= 0 dif) 8 dif) :quantity (get times (first x))})))

;; Given a line of raw forge data, builds map containing formatted data
(defn forge-data->map [line m fd] ;; where m is forge (map defined from header)
  (let [sm (filter-times line (:time-start m))]
    (when (<= (:time-start m) (count line))
      (->
       (reduce conj (for [k (keys m)] (assoc {} k (nth line (k m)))))
       (assoc :time-start (:time-start m))
       (assoc :times (->forge-quantity-at-time sm fd))))))
                                      
;; Builds forge data from FORGE filename
(defn fm->data [filename]
  (let [lines (csv->lines filename)]
   (for [line lines]
     (when (< (:time-start forge) (count line)) 
       (forge-data->map line forge lines)))))

;; Function to get force code from file name for forge data 
(defn forge-file->fc [forge-file]
  (first (clojure.string/split
          (last (clojure.string/split forge-file
                                      #"FORGE_")) #".txt")))

;; Function to get forge-file based on force code and standard naming convention
(defn fc->forge-file [fc root]
  (str root "FORGE_" fc ".txt"))
 ;(str (if root (str (first root) "\\") "") "FORGE_" fc ".txt"))

;; Reads forge file and puts data into map and adds force-code
(defn forge->data [filename vcons-data]
  (let [fc (forge-file->fc filename) data (fm->data filename)]
    (for [d data]
      (assoc 
        (assoc d :force-code fc)
        :title_10 "10"))))
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ===== HELPER FUNCTIONS SPECIFIC TO FORCE CODES =============================
;; ============================================================================
(defn check-fc [data c]
  (= c (str (first (:force-code data)))))

;; Checks if is vignette/scenario based on force-code value
(defn scenario? [data] (= "SE" (apply str (take 2 data))))
(defn vignette? [data] (not (scenario? data))) ;;No naming convention for vignettes, only scenarios have to start with SE

;; Filters out classification labels ((U) or (S), ect) when used infront force-code label 
(defn filter-fc [x]
  (first (filter #(not= "" %) (clojure.string/split x #"\([A-Z]\) "))))
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ===== FUNCTION USED TO FORMAT MAP DATA =====================================
;; ============================================================================

;; tdata->map and tmap->data used to be used for both FORGE data and MAP data, but now only used for MAP

;; Given a line read from csv file and constant map (forge or v-map),
;; Returns the values fommated into map
;; Only used for vignette map
(defn tdata->map [line m]
  (let [sm (filter-times line (:time-start m)) seqs (get-seqs (sort (keys sm)))]
    (when (<= (:time-start m) (count line))
      (->
       (reduce conj (for [k (keys m)] (assoc {} k (nth line (k m)))))
       (assoc :time-start (:time-start m))
       (assoc :times (flatten (for [q seqs] (for [t (->split (sub-map sm q)) :let [ks (sort (keys t))]]
                                              {:start (first ks) :duration (- (last ks) (first ks))
                                               :quantity (get t (first ks))}))))))))

;; Given map m, formats time data for all lines in file (used for vignette map data)
(defn tmap->data [filename m] 
  (let [data (file->map filename)]
    (filter #(identity %)
            (for [line (keys data)]
              (tdata->map (get data line) m)))))

;; Builds vignette map from file and returns data formmated into map
;; USING OLD MAP FILE FORMAT
;(defn vmap->data [filename]
;  (let [tm (tmap->data filename v-map)]
;    (reduce conj
;            (for [m tm] (assoc {} (:force-code m) m)))))

;; When format of MAP file changes use this: 
(def new-map (build-index ["force-code" "title" "bct-old" "bct-new" "times"]))

(defn vmap->data [filename]
  (let [lines (partition-by #(identity (first %)) (csv->lines filename))]
    (zipmap
     (map ffirst lines)
     (for [line lines :let [f (first line)]]
       (assoc 
        (zipmap (keys new-map) (take 4 f))
        :times (for [t line]
                 (zipmap [:start :duration :quantity :v] (drop 4 t))))))))
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ====== FUNCTIONS TO BUILD VIGNETTE CONSOLIDATED DATA =======================
;; ============================================================================

;; Helper function: takes line of considate data and formats into map
(defn vcons->map [cline]
  (reduce conj
          (for [k (keys v-cons)
                :let [data (if (zero? (k v-cons))
                             (filter-fc (nth cline (k v-cons)))
                             (if (> (count cline)
                                    (k v-cons)) (nth cline (k v-cons)) ""))]]
            (assoc {} k data))))

;; Builds map from vignette consolidate file 
(defn vcons->data [filename]
  (let [data (file->vecmap filename
                           #(filter-fc (nth % (:force-code v-cons)))
                           #(nth % (:src v-cons)))]
    (for [cline (keys data)]
      (vcons->map (get data cline)))))
;; ============================================================================
;; ============================================================================


 


;; ============================================================================
;; ===== FUNCTIONS TO FORMAT MERGE DATA FROM DIFFERNT FILES TOGETHER ==========
;; ============================================================================

;; Merges vignette data and forge/vignette consilidated data, forge data requires force-code arg to be passed
(defn merge-vignette [vm-data m & fc]
  (let [fc (if fc (first fc) (:force-code m))]
    (merge (get vm-data fc) m)))

;; Expands vignette records for each time the data is split
(defn expand-vignette [vmerged vm]
  (for [time (:times vmerged) :let [ttl (:title (get vm (:force-code vmerged)))]]
    ["DemandRecord" ;; Type
     "TRUE" ;; Enabled
     "1" ;; Priority
     (:quantity vmerged) ;; Quantity
     "1" ;; Demand Index
     (:start time) ;; Start day
     (:duration time) ;; Duration
     "45" ;; Overlap
     (:src vmerged) ;; SRC
     "Uniform" ;; SourceFirst
     ;; Demand Group
     (if (= "S-" (subs (:force-code vmerged) 0 2)) (:force-code vmerged) "Ungrouped") 
     (:force-code vmerged) ;; Vignette 
     (if (or (= "" ttl) (nil? ttl)) (:force-code vmerged) ttl) ;; Operation
     (if (= "H" (str (first (:force-code vmerged)))) "NonBOG" "Rotational") ;; Catagory
     (:title_10 vmerged) ;; Title_10 
     (:title vmerged) ;; IO_Title
     (:str vmerged)])) ;;People/Strength per SRC

;; Expands forge records for each time the data is split
(defn expand-forge [f fd t0]
  (for [time (:times f)]
    ["DemandRecord" ;; Type
     "TRUE" ;; Enabled
     "1" ;; Priority
     (:quantity time) ;; Quantity
     "1" ;; Demand Index
     (str (+ t0 (:start time))) ;; Start day - added offset t0 from start time of scenario in v-map
     (if (= 0 :duration time) "8" (:duration time)) ;; Duration
     "45" ;; Overlap
     (:src f) ;; SRC
     "Uniform" ;; SourceFirst
     (:force-code f) ;; Demand Group
     (:force-code f) ;; Vignette
     (get-phase-from-day (:start time) fd) ;; Operation - where time is the forge time, not total time
     "Rotational" ;; Catagory
     (:title_10 f) ;; Title_10
     (:title f)])) ;; IO_Title 
;; ============================================================================
;; ============================================================================

;;Removes the last value value of seq and adds newlast (does not gaurantee seq remains ordered)
(defn replace-last [seq newlast] (conj (take (dec (count seq)) seq) newlast))

(def edited-forge-srcs (atom #{}))

;;Replaces the 6th value (duration) with d0 in line
(defn insert-new-duration [line d0 t0 fd]
  (let [t (- (read-string (str (nth line 5))) t0)
        e (apply max (vals (phase-indx fd)))]
    (if (>= e t) ;;only replace when line is in the last phase. 
      line
      (let [newline (concat (conj (vec (take 6 line)) d0) (vec (take-last 10 line)))
            _ (swap! edited-forge-srcs conj newline)] newline))))
        
        

;; Expands forge records for each time the data is split
(defn expand-forge [f fd t0 tf]
  (let [lines (for [time (:times f)]
                ["DemandRecord" ;; Type
                 "TRUE" ;; Enabled
                 "1" ;; Priority
                 (:quantity time) ;; Quantity
                 "1" ;; Demand Index
                 (str (- (+ t0 (:start time)) 1)) ;; Start day - added offset t0 from start time of scenario in v-map
                 (if (= 0 :duration time) "8" (:duration time)) ;; Duration
                 "45" ;; Overlap
                 (:src f) ;; SRC
                 "Uniform" ;; SourceFirst
                 (:force-code f) ;; Demand Group
                 (:force-code f) ;; Vignette
                 (get-phase-from-day (:start time) fd) ;; Operation - where time is the forge time, not total time
                 "Rotational" ;; Catagory
                 (:title_10 f) ;; Title_10
                 (:title f) ;; IO_Title 
                 (:str f)]) ;;People/Strength per SRC 
        sorted-lines (sort-by #(read-string (nth % 5)) lines)
        newline (insert-new-duration (last sorted-lines) (- tf (read-string (nth (last sorted-lines) 5))) t0 fd)]
    (replace-last sorted-lines newline))) 

;; ============================================================================
;; ===== FUNCTIONS TO WRITE MERGED DATA TO FILE ===============================
;; ============================================================================

;; Converts vignette consolidate data to list of formatted vectors
(defn vcons->lines [v vcons]
  (apply concat
         (for [c vcons]
           (expand-vignette (merge-vignette v c) v))))

;; The offset time, t0, is the time the scenario starts in the v-map. 
;; Each time in the forge data has a forge time with the overall offset of t0. 
(defn get-offset [f vmap]
  (let [t0 (apply min (map #(:start %) (:times (get vmap (:force-code f)))))]
    (if (nil? t0) 0 t0)))

;; The last time period in forge should end on the last time period specified by the map. 
;; If there is a difference in the map end time for a scenario and the end time in the forge data,
;;  the final time period in the forge file needs to be changed to the final time from the map.
(defn get-final-map-time [f vmap expand]
  ;(println (str expand "\t" f))
  ;;only expand if it is the last period in the last phase and the map duration is greater than the forge duration
  (if expand
    (apply max (map #(+ (read-num (:start %)) (read-num (:duration %))) (:times (get vmap (:force-code f)))))
    (dec (+ (last (map #(read-num (:start %)) (:times (get vmap (:force-code f)))))
          (apply max (map #(+ (:start %) (:duration %)) (:times f)))))))

(defn forge-end-day
  "Given forge data (fd), returns the last day of the file."
  [fd]
  (-> (re-find day-regex (reduce str (last (forge-days fd))))
      (clojure.string/split  #" ")
      (second)
      (bigdec)
      (long)))

;; Converts forge data to list of formatted vectors
(defn forge->lines [forges fd vmap]
  (let [last-day (forge-end-day fd)
        lines (apply concat (for [f (filter #(and (not= "SRC" (:src %)) (not= "" (:src %)) (not= nil (:src %))) forges)]
                              ;;filter out non-data rows
                              (expand-forge f fd (read-num (get-offset f vmap)) 
                                            (if (> (last (map #(+ (:start %) (:duration %)) (:times f))) last-day)
                                              (get-final-map-time f vmap true)
                                              (get-final-map-time f vmap false)))))]  ;;t0 passed as argument to expand-forge 
    lines))
                             

;; Uses vignette map to create list of demands from forge files
(defn vmap->forge-demands [vm vcons root]
  (apply concat 
    ;;need to remove entries that don't have any demands (all blank cells for demands) in FORGE file     
    (for [fc (filter #(= "SE" (apply str (take 2 %))) (keys vm))
          :let [fdata (forge->data (fc->forge-file fc root) vcons)]]
      (forge->lines fdata (csv->lines (fc->forge-file fc root)) vm))))

;; Uses vignette map to create list of demands from vignette consolidated data
(defn vmap->vignette-demands [vm vcons]
  (vcons->lines vm (filter #(vignette? %) vcons))) ;;vignetees no longer have to follow any naming convention. 

;; Builds list of all demands from vignette file and vignette consolidate file
(defn build-demand [vfile cfile root]
  (let [vm (vmap->data vfile) vcons-data (into [] (vcons->data cfile))] ;; need this to not be lazy 
    (into (vmap->forge-demands vm vcons-data root) (vmap->vignette-demands vm vcons-data))))
;; ============================================================================
;; ============================================================================



;; ============================================================================
;; ===== FUNCTIONS TO AUTOMATE MAKING DEMAND FILES GIVEN THE ROOT DIR =========
;; ===========================================================================

(def output-headers ["Type" "Enabled" "Priority" "Quantity" "DemandIndex" "StartDay" "Duration" "Overlap" "SRC" "SourceFirst" "DemandGroup" "Vignette" "Operation" "Category" "Title 10_32" "OITitle" "Strength"])

;; Writes list of demands to outfile 
(defn demands->file [demands outfile]
  (with-open [w (io/writer outfile)]
    (doseq [line (into [output-headers] demands)]
      ;;(println line)
      (doseq [d line]
        (write! w (str d "\t")))
        ;;(write! w (str d)) (write! w "\t")) 
      (writeln! w ""))))

;; Returns true when file is filetype 
(defn is-filetype? [filename filetype]
  (= 2 (count (clojure.string/split filename (re-pattern filetype)))))

(defn vcons-file? [filename] (is-filetype? filename "CONSOLIDATED_"))
(defn vmap-file? [filename] (is-filetype? filename "MAP_"))
(defn forge-file? [filename] (is-filetype? filename "FORGE_"))

;; Gets filenames of filetype from root dir 
(defn root->filetype [root fn-type]
  (filter fn-type (map fname (list-files root))))

(defn find-file [root f]
  "returns the path of file from root when filepaths are filtered by f"
  (str root "\\" (first (root->filetype root f))))

;; Creates Demand records from files in the root directory
;; If more than 1 vignette map or vignette consolidated file is in the directory, only the first one is used
(defn root->demand-file [root & outfile]
  (when (nil? root)
    (do (println "No file selected.") (->frame "No files selected." "No Input files selected.") (throw (Exception. "NoInputFilesSelectedException"))))
  (let [outfile (if outfile (first outfile) (str (last (clojure.string/split root #"[\\ | /]")) "_DEMAND.txt"))
        ;; If multiple maps or consolidate files, will only use the first one
        vfile (find-file root vmap-file?)
        cfile (find-file root vcons-file?)]
    (try 
      (demands->file (build-demand vfile cfile root) (str root "/" outfile))
      (println (str "Created demand file "(str root (last (clojure.string/split root #"[\\|/]")) "_DEMAND.txt")))
      (with-open [w (io/writer (str root "edited-forge-srcs.txt"))]
        (doseq [line @edited-forge-srcs]
          (doseq [val line]
            (write! w (str val "\t"))) (writeln! w ""))
        (.close w))
      
      (->frame "File Created" (str "Demand File Created at:\n" 
                                   (str root (last (clojure.string/split root #"[\\|/]")) "_DEMAND.txt")
                                   "\n\nInputs Used:\n" vfile "\n" cfile))
      (demand_builder.chart/demand-file->sand-charts (str root (last (clojure.string/split root #"[\\|/]")) "_DEMAND.txt") :save true :view true)
      
      
      (catch java.io.FileNotFoundException e
        (println e)
        (println (str "Could not find demand inputs at " root))
        (->frame "No Inputs Found" (str "Could not find all inputs at root: " root 
                                        "\n\nFile Not Found At:\n" (.getMessage e)
                                        "\n\nVignette Consolidated File:\t"(fname cfile)"\nVignette Map File:\t"(fname vfile)))))))                                
;; Calls root->demad for multipile roots
(defn roots->demand-files [roots]
  (let [roots (if (string? roots) [roots] roots)]
    (doall (pmap #(root->demand-file %) roots))))
  
    

;; Makes file select window appear
(defn choose-file []
  (let [f (javax.swing.JFrame.)
        c (javax.swing.JFileChooser.)]
    (.add f c)
    (.setDialogTitle c "Inputs to Demand Files")
    (.setFileSelectionMode c JFileChooser/DIRECTORIES_ONLY)
    (.setMultiSelectionEnabled c true)
    (let [x (.showOpenDialog c f)]
      (if  (zero? x)
        (map #(.getPath ^File %) (.getSelectedFiles c))
        (println "No file selected.")))))

(defn ->demand-file 
  ([] (roots->demand-files (map #(str % "/") (choose-file))))
  ([roots] (roots->demand-files roots)))
;; ============================================================================
;; ============================================================================





;; ============================================================================
;; ===== MAIN METHOD FOR STANDALONE JAR =======================================
;; ============================================================================
"
Format: 
All input files have to be formatted as tab delimited .txt files
Scripts assumes files have the following naming conventions:
Vignette Map: MAP_filename
Vignette Consolidated: CONSOLIDATED_filename
For Each File: FORGE_[FC], where [FC] is the value for the first entry in Vignette map. 

Scenarios have to be have the label SE as force code id prefix
Force code prefixes that start with 'V-' 'S-' or 'H ... -' will have data read from CONSOLIDATED file.

Note: quantity in Vignette map is never used,
      quantity values from either Forge file or Consolidated file

Script assumes there is only one map and one consolidated file in root directory.
If there are more than one, only the first one will be used.

To run, use ->demand-file function to run. When a single path or a collection passed in as arguments,
Creates demand files for each path. 

When no argument passed in, opens GUI to select path/paths (can select multiple paths)
"
(comment
  (defn -main [& args]
    (binding [*closeon* 3]
      (if (nil? args)
       (->demand-file)
       (->demand-file args)))))

(require 'demand_builder.gui)
(defn -main [& args]
  (demand_builder.gui/main-gui :exit true))




;; ============================================================================
;; ============================================================================


;; Testing

(comment (def inr "K:\\Divisions\\FS\\repos\\taa_test_data\\demand_builder\\complexest\\Input\\")
 (root->demand-file inr)
 ;;NullPointerException   clojure.lang.Numbers.ops (Numbers.java:1013)
 (def nr "K:\\Divisions\\FS\\repos\\taa_test_data\\demand_builder\\complexest_trial_n_error\\Input\\")
 (root->demand-file nr)

 (def vs [{:force-code "Event Code", :src2 "SCR2", :src "SRC", :title "SRC TITLE`", :str "STR", :quantity "QTY", :title_10 "Title 10_32", :non-rot ""} {:force-code "S-7337", :src2 "10", :src "10473R100", :title "QM COMP SPLY CO", :str "208", :quantity "2", :title_10 "10", :non-rot ""} {:force-code "S-706", :src2 "8", :src "08670R000", :title "MED LOG MGMT CTR", :str "14", :quantity "1", :title_10 "10", :non-rot ""}])

 (def vins "K:\\Divisions\\FS\\repos\\taa_test_data\\demand_builder\\complexest_trial_n_error\\Input\\FORGE_SE-99.txt"))

;; ============================================================================
;; ===== ALTERNATIVE METHOD OF FORMATTING FORGE DATA ==========================
;; ============================================================================

;; This method of formatting the FORGE data uses the 'UNIT_NODE_DETAIL' sheet
;; in the FORGE file. The current method uses the 'SRC_By_Day' sheet

;; To switch from using 'SCR_By_Day' sheet to 'UNIT_NODE_DETAIL' sheet,
;; uncomment and move this section to the FORGE formatting section 
;; and replace the functions 'fm->data' and 'expand-forge'

(comment ;; uncomment and move if using alternative formatting method
;; Reads the first line of file
 (defn read-header [filename]
   (with-open [r (clojure.java.io/reader filename)]
     (clojure.string/split (first (line-seq r)) #"\t")))

;; Reads all forge data and sorts by src
 (defn read-forge [filename]
   (let [fm (build-index (read-header filename))]
     (sort-by #(nth % (get fm :SRC)) (csv->lines filename))))

;; Function to return an attribute from a forge record (fr)
;; Given the forge index map and a string as the key (use string, not keyword, as key)
 (defn get-a [fr fm key]
   (nth fr (get fm (keyword key))))

;; Splits all forge records by a supplied key
 (defn split-by-key [fdata fm key]
   (partition-by #(identity (nth % (get fm key))) fdata))

;; Splits all forge records by src
 (defn split-by-src [fdata fm]
   (split-by-key fdata fm :SRC))

;; Given a subset of forge records that have been split by src,
;; Returns a map with the a coll of unique values for start, end, quantity, and phase
 (defn ->ftimes [fd fm]
   (into #{}
         (for [f fd]
           (zipmap [:start :end :quantity :phase]
                   (map #(get-a f fm %) ["Time Period Begin Day"
                                         "Time Period End Day"
                                         "UIN Quantity"
                                         "Subphase"])))))

;; Given all forge data, splits the data by scr and gets the unique data for each src
;; Returns a single map with the non-unique keys, and coll of map for unique keys
 (defn ->fdata [fd fm fc]
   (for [src (split-by-src fd fm) :let [f (->ftimes src fm)]]
    {:src (get-a (first src) fm "SRC")
      :title (get-a (first src) fm "Title")
      :force-code fc
      :times f}))

;; Formats all forge data given the filename
;; Returns a coll of maps for all forge records,
;; Similar to the inital method of formatting forge data
;; The only differece between this and inital method is that
;; Phase information is a supplied field within the times key
;; and does not have to be derived
 (defn fm->data [filename]
   (->fdata (drop 1 (csv->lines filename))
            (build-index (read-header filename))
            (forge-file->fc filename)))

;; Corrected expand-forge method for new method of processing forge data
;; Only change is the operation field which is retrived from times
;; Rather than derived from the header information

;; Expands forge records for each time the data is split
 (defn expand-forge [f fd]
   (for [time (:times f)]
     ["DemandRecord" ;; Type
      "TRUE" ;; Enabled
      "1" ;; Priority
      (:quantity time) ;; Quantity
      "1" ;; Demand Index
      (:start time) ;; Start day
      (- (read-num (:end time)) (read-num (:start time))) ;; Duration
      "45" ;; Overlap
      (:src f) ;; SRC
      "Uniform" ;; SourceFirst
      (:force-code f) ;; Demand Group
      (:force-code f) ;; Vignette
      (:phase time) ;; Operation
      "Rotational" ;; Catagory
      "10" ;; Title_10
      (:title f)]))) ;; IO_Title 

 ;; end of comment block
;; ============================================================================
;; ============================================================================
