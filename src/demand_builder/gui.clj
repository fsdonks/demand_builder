(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f] [extend :as ex] [phaseshifter :as ps] [m4plugin :as m4p]]
            [spork.util [io :as io] [table :as tbl]]
            [spork.util.excel [core :as exl]]
            [seesaw core]
            [clojure.java [io :as jio]])
  (:import [java.io File FileNotFoundException]
           [javax.swing BoxLayout JPanel JTextField JTextArea])
  (:use [seesaw core chooser]))

;;The demand builder gui is no long in use.  I had deleted the
;;pipeline for the gui data processing in demand_builder.core because
;;it was a separate workflow from the tests and taa repo production
;;pipeline that we have been using in demand_builder.formatter.

;;For now, we will save this workspace in case we ever want to hook up
;;another gui to the production pipeline.

(native!)
(set! *warn-on-reflection* true)

(defn select-file []
  (seesaw.chooser/choose-file :type :open :selection-mode :files-and-dirs 
    :success-fn (fn [fc file] (.getAbsolutePath ^java.io.File file))))

(defn map->inputfile [m]
 (let [out (clojure.string/replace (:Path (first m))  (io/fname (:Path (first m)) ) "input-map.txt")]
   (spork.util.stream/records->file m out) out))

(defn file-choice->root [f] (when f (if (.isDirectory (^java.io.File io/file f)) f (io/fdir f))))
(defn root->outputdir [root] (str (io/as-directory root) (io/as-directory m4p/outputdir)))
(defn outdir->demandfile [outdir] (str (io/as-directory outdir) (io/fname outdir) "_DEMAND.txt"))
(defn file->pdir [^java.io.File f] (if (.isDirectory f) (str f) (io/as-directory (.getParent f)))) ;;If file is directory, return directory, otherwise, return parent of file
(defn file-type [root type] (filter #(clojure.string/includes? % type) (map str (io/list-files root))))
(defn is-scenario? [v] (= "SE" (apply str (take 2 v))))

(defn text-area [txt & {:keys [x y] :or {x 600 y 600}}]
  (let [t (^javax.swing.JTextArea JTextArea. (str txt))]
    (do (.setRows t 100) (.setSize t x y) (.setLineWrap t true)) t))

(defn y-panel [] (let [p (^javax.swing.JPanel JPanel.)] (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))  p))
(defn text-field [& {:keys [size] :or {size 7}}] (let [tf (JTextField.)] (.setColumns tf size) tf))
(defn show-frame [^javax.swing.JFrame f ^javax.swing.JPanel p & {:keys [x y] :or {x 100 y 100}}] 
  (doto f (.add p) (.setSize x y) (.setVisible true)) nil)

(defn addp [^javax.swing.JPanel p ^java.awt.Component c] (.add p c))
(defn addm [^javax.swing.JFrame f ^javax.swing.JPanel p] (.add f p))

(defn get-text [c] (cond (instance? javax.swing.JTextField c) (.getText ^javax.swing.JTextField c)
                         (instance? javax.swing.JLabel c) (.getText ^javax.swing.JLabel c)))

(defn set-text [c s] (cond (instance? javax.swing.JTextField c) (.setText ^javax.swing.JTextField c s)
                       (instance? javax.swing.JLabel c) (.setText ^javax.swing.JLabel c s)))

(defn hide [^javax.swing.JFrame f] (.setVisible f false))

;;Creates gui with txt (txt = .getStackTrace of exception)
(defn error-gui [txt]
  (let [f (frame :title "Error StackTrace") p (y-panel) t (text-area txt)]
    (do (addp p t) (show-frame f p :x 500 :y 200))))

;;Returns button that calls function f
;;Creates error gui for any error messages. Re-throws exceptions
(defn fn->button [f title] 
  (let [b (button :text title)]
    (listen b :action (fn [e] (try (f) 
                                (catch Exception e
                                  (do (println e) (error-gui (.getMessage e)) (throw (ex-info (.getMessage e) {:input 42})))))))
    b))

(defn vec->line [v] (str (apply str (map #(str % "\t") v)) "\n"))
(defn spit-tsv [filename lines header]
  (if (io/fexists? filename)
    (jio/delete-file filename))
  (spit filename header)
  (doseq [l lines] (spit filename l :append true)))

;;Returns the filename of inputs found in root
(defn inputs-used [root]
  (let [mapfile (first (file-type root "MAP_"))
        confile (first (file-type root "CONSOLIDATED_"))
        forges (file-type root "FORGE_")]
    {:MAP mapfile :CONS confile :FORGE forges}))

;;Shows list of files that where used to build demand
(defn inputs-used-gui [root]
  (let [files (inputs-used root) p (y-panel) fn->lbl->p #(addp p (label (io/fname %)))]
    (doall (map #(fn->lbl->p (% files)) [:MAP :CONS]))
    (doseq [forge (:FORGE files)] (fn->lbl->p forge))
    (show-frame (frame :title "Inputs Used") p :x 350 :y 100)))

;;Builds a form gui showing the file mappings that will be used to build demand file
(defn confirm-input-map [root]
  (m4p/root->inputmap root)  ;;build the input map from root
  (let [input-file (str (io/as-directory root) "input-map.txt")
        h ["Path" "Type" "ForceCode" "Sheetname"]
        data (into [] (tbl/tabdelimited->records input-file :keywordize-fields? false))
        f (frame :title "Confirm Demand Builder Inputs")
        main (y-panel)
        entries (doall (for [d data :let [p (y-panel)]]
                         (zipmap h [(label (get d "Path")) (text (get d "Type")) (text (get d "ForceCode")) (text (get d "Sheetname"))])))]
    (doseq [e entries] (doseq [k h] (addp main (get e k))))
    (addp main (fn->button #(do (spit-tsv input-file (map vec->line (for [e entries] (for [k h] (get-text (get e k))))) (vec->line h))
                              (m4p/inputfile->demand input-file)                              
                              (inputs-used-gui (root->outputdir root))
                              (hide f)) "Run"))
    (show-frame f main :x 600 :y 300)))

(defn print-phases [f]
  (let [phases (ps/demands->phases (ps/read-demand-file f) (ps/read-header f))]
    (for [p (filter is-scenario? (keys phases)) :let [r (get phases p)]]
      (str "\n" (str p "\tStart\tEnd") "\n" (apply str (map #(str (second %) "\t" (nth % 2) "\t" (+ (nth % 2) (nth % 3)) "\n") r))))))

(defn phases->gui [file]
  (let [f (frame :title "Phase Timings") p (y-panel) t (text-area (apply str (print-phases file)))] 
    (do (addp p t) (show-frame f p :x 220 :y 300))))

(defn phase-shifter-gui [root]
  (let [file (str root (spork.util.io/fname root) "_DEMAND.txt")
        data (c/file->map-list file)
        data->str (fn [seq title] (str title (apply str (map #(str "<br>" %) seq))))
        scenarios (filter is-scenario? (distinct (map :DemandGroup data)))
        phases (distinct (map :Operation (filter #(is-scenario? (:Vignette %)) data)))
        frame (frame :title "Phase Shifter")
        mainpanel (JPanel.) scenariopanel (y-panel) phasepanel (y-panel) deltapanel (y-panel) header (y-panel)
        objs (doall (for [i (range 10)] {:scenario (text-field :size 10) :phase (text-field) :change (text-field)}))
        panels [scenariopanel phasepanel deltapanel]]
    (doall (map #(set-text ((first %) (first objs)) (second %)) [[:scenario "DemandGroup"] [:phase "Phase"] [:change "Change"]]))
    (doseq [o objs] (addp scenariopanel (:scenario o)) (addp phasepanel (:phase o)) (addp deltapanel (:change o)))
    (doseq [p panels] (addp mainpanel p) (addm frame mainpanel))
    (addp header (label (str "<html>" (data->str scenarios "Scenarios: ") "<br>" (str "<br>" (data->str phases "Phases: ")))))
    (addp mainpanel header)
    (addp mainpanel (fn->button (fn [] (phases->gui file)) "Phase Timings"))
    (addp mainpanel (fn->button (fn [] (let [lines (doall (for [o objs] [(get-text (:scenario o)) (get-text (:phase o))
                                                                         (let [t (get-text (:change o))] (try (c/read-num t) (catch Exception e t)))]))]
                                         (spit (str root "changes.txt") (reduce str (map #(str (first %) "\t" (second %) "\t" (last %) "\n") (filter #(not= "" (first %)) lines))))
                                         (ps/->output-new-phases file (ps/read-header file) (str root "changes.txt") file)
                                         (clojure.java.io/delete-file (str root "changes.txt"))
                                         (hide frame))) "Shift Phases"))
    (show-frame frame mainpanel :x 432 :y 276)))

;;This gui will allow users to enter values to shift the start/duration of demand groups in a demand file
(defn extender-gui [filename]
  (let [f (frame :title "Extender")
        mainpanel (y-panel)
        panels (zipmap [:groupp :startp :durationp :startdelta :durationdelta] (repeat 5 (JPanel.)))
        times (ex/get-times filename "DemandGroup")
        objs (doall (for [i times :let [c (vec (zipmap (keys panels)
                                                 (flatten [(map #(label (str %)) ((juxt first second last) i)) (text-field ) (text-field)])))]]
                      (do (doall (map #(addp ((first %) panels) (second %)) c))
                        (zipmap (map first c) (map second c)))))]
    (doseq [p (vals panels)] (addp mainpanel p))
    (addp mainpanel (fn->button #(do (ex/updates->file filename filename 
                                       (flatten (doall (for [o objs :let [num (fn [t] (if (not= "" t) (c/read-num t) 0))]]
                                                         (map (fn [x] {:valkey (first x) :groupkey :DemandGroup 
                                                                       :key (get-text (:groupp o)) :delta (num (get-text ((second x) o)))})
                                                           [[:StartDay :startdelta] [:Duration :durationdelta]])))))
                                   (hide f)) "Shift Start Times"))
    (show-frame f mainpanel :x 276 :y 150)))

(defn refresh-frame [f p] (.updateUI ^javax.swing.JPanel p)) 
(defn remove-from-panel [p c] (.remove ^javax.swing.JPanel p ^java.awt.Component c))

;;Gets all unique the ForceCodes from the MAP file
(defn excel-map->fcs [efile sheetname]
  (let [possible-keys (conj (map first (filter #(= "ForceCode" (second %)) (vec m4p/header-map))) "ForceCode")
        data (get (exl/xlsx->tables efile :sheetnames [sheetname]) sheetname)
        i (first (filter identity (map #(get (zipmap (:fields data) (range (count (:fields data)))) %) possible-keys)))]
    (nth (:columns data) i)))

(defn add-scenario [root a]
  (let [m (first (filter #(= "MAP" (:Type %)) @a))
        fcs (filter is-scenario? (excel-map->fcs (:Path m) (:Sheetname m)))
        f (frame :title "Add Scenario")
        p (y-panel)
        file-options (listbox :model (clojure.set/difference (set (m4p/list-excel-files root)) (set (map :Path @a))))
        sheet-options (listbox :model ["Unit_Node_Detail" "SRC_By_Day"])
        fc-options (listbox :model (clojure.set/difference (set fcs) (set (map :ForceCode @a))))
        b (fn->button #(do (hide f)
                           (swap! a conj  
                             {:Type "FORGE" :Path (selection file-options) 
                              :Sheetname (selection sheet-options) :ForceCode (selection fc-options)})) "Add Scenario")
        options? #(and (selection fc-options) (selection file-options) (selection sheet-options))]
    (doseq [c [file-options sheet-options fc-options]]
      (listen c :selection (fn [e] (when (options?) (do (addp p b) (refresh-frame f p))))))              
    (doseq [c [(label "File Path") file-options (label "Sheet") sheet-options (label "ForceCode") fc-options]] (addp p c))
    (show-frame f p :x 500 :y 200)))

(defn ->file-metadata-gui [root a type]
  (let [f (frame :title type)
        p (y-panel)
        file-options (listbox :model (clojure.set/difference (set (m4p/list-excel-files root)) (set (map :Path @a))))
        sheet-options (atom (listbox :model []))
        run-button (fn->button #(when (and (selection @sheet-options) (selection file-options))
                                  (do (hide f) 
                                    (swap! a conj 
                                      {:Type type :Path (selection file-options) :Sheetname (selection @sheet-options)
                                       :ForceCode "none"})))
                     (str "Use this file as " type))]
    (listen file-options :selection (fn [e]
                                      (remove-from-panel p @sheet-options) (remove-from-panel p run-button)
                                      (reset! sheet-options (listbox :model (m4p/list-sheets (selection file-options))))
                                      (addp p @sheet-options) (addp p run-button) (refresh-frame f p)))
    (doseq [c [(label "File Path") file-options (label "Sheet") @sheet-options]] (addp p c))
    (show-frame f p :x 500 :y 200)))

;;Gui to promt user to select files and options to generate meta data file, then run demand-builder
(defn ->enter-meta-data []
  (let [a (atom [])
        root (select-file)
        f (frame :title "Select Inputs")
        filef (frame :title "Files Used")
        p (y-panel)
        main (y-panel)
        h [:Path :Type :ForceCode :Sheetname]
        cons-button (fn->button #(->file-metadata-gui root a "CONSOLIDATED") "Add CONSOLIDATED file")
        scenario-button (fn->button #(add-scenario root a) "Add FORGE file")
        map-button (fn->button #(do (->file-metadata-gui root a "MAP") 
                                  (addp main cons-button)
                                  (addp main scenario-button)
                                  (refresh-frame f main)) "Add MAP file")
        show-files (fn->button 
                     #(do (.removeAll ^javax.swing.JPanel p)
                        (doseq [i @a] (doseq [k h] (addp p (if (= k :Path) (label (get i k)) (text (or (get i k) "none"))))))
                        (refresh-frame filef p)
                        (show-frame filef p :x 600 :y 300)) "Files Selected")
        run-button (fn->button #(do (let [inputfile (str (io/as-directory root) "input-map.txt")]
                                      (do (spork.util.stream/records->file @a inputfile))
                                      (do (m4p/setup-directory inputfile))
                                      (m4p/inputfile->demand inputfile)
                                      (hide f))) "Run Demand Builder")]
    (addp main show-files)
    (addp main run-button)
    (addp main map-button)
    (show-frame f main :x 300 :y 160)))

;;Buttons -> first is fn second is title
;;Set Working Dir, Set Existing Dir, Shift Phases, Shift DemandGroup, Sand Chart 
(defn ->buttons [lbl]
  (map #(fn->button (first %) (second %)) 
    [[#(let [root (file-choice->root (select-file))]
         (when (io/fexists? root)
           (set-text lbl (root->outputdir root)) (confirm-input-map root)))  "Set Working Directory"]
     [#(let [f (choose-file)] (when (pos? (count (apply concat (vals (inputs-used (file->pdir f))))))
                               (set-text lbl (file->pdir f)))) "Set Existing Demand"]
     [#(->enter-meta-data) "Manually Set Meta Data"]
     [#(phase-shifter-gui (get-text lbl)) "Shift Phases"]
     [#(extender-gui (outdir->demandfile (get-text lbl))) "Shift DemandGroup"]
     [#(c/demand-file->sand-charts (outdir->demandfile (get-text lbl)) :view true :save false) "Sand Chart"]]))

(defn main-gui [& {:keys [exit] :or {exit false}}]
  (let [f (frame :title "Demand Builder") p (y-panel) rdl (label "No working directory set")]
    (when exit (.setDefaultCloseOperation ^javax.swing.JFrame f javax.swing.JFrame/EXIT_ON_CLOSE))
    (doseq [c (flatten (list rdl (->buttons rdl)))] (addp p c))
    (show-frame f p :x 366 :y 200)))

(def root "C:\\Users\\michael.m.pavlak.civ\\Desktop\\complexest\\Input\\Excel")

