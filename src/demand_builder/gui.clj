(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f] [extend :as ex] [phaseshifter :as ps] [m4plugin :as m4p]]
            [spork.util [io :as io] [table :as tbl]]
            [seesaw core]
            [clojure.java [io :as jio]])
  (:import [java.io File FileNotFoundException]
           [javax.swing BoxLayout JPanel JTextField])
  (:use [seesaw core chooser]))

(native!)
(defn select-file []
  (seesaw.chooser/choose-file :type :open :selection-mode :files-and-dirs
    :success-fn (fn [fc file] (.getAbsolutePath file))))

(defn map->inputfile [m]
 (let [out (clojure.string/replace (:Path (first m))  (io/fname (:Path (first m)) ) "input-map.txt")]
   (spork.util.stream/records->file m out) out))

(defn file-choice->root [f] (when f (if (.isDirectory (io/file f)) f (io/fdir f))))
(defn root->outputdir [root] (str (io/as-directory root) (io/as-directory m4p/outputdir)))
(defn outdir->demandfile [outdir] (str (io/as-directory outdir) (io/fname outdir) "_DEMAND.txt"))
(defn file->pdir [f] (if (.isDirectory f) (str f) (io/as-directory (.getParent f)))) ;;If file is directory, return directory, otherwise, return parent of file
(defn file-type [root type] (filter #(clojure.string/includes? % type) (map str (io/list-files root))))
(defn is-scenario? [v] (= "SE" (apply str (take 2 v))))

(defn y-panel [] (let [p (javax.swing.JPanel.)] (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))  p))
(defn text-field [& {:keys [size] :or {size 7}}] (let [tf (JTextField.)] (.setColumns tf size) tf))
(defn show-frame [f p & {:keys [x y] :or {x 100 y 100}}] (doto f (.add p) (.setSize x y) (.setVisible true)) nil)

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
  (let [files (inputs-used root) p (y-panel) fn->lbl->p #(.add p (label (io/fname %)))]
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
    (doseq [e entries] (doseq [k h] (.add main (get e k))))
    (.add main (fn->button #(do (spit-tsv input-file (map vec->line (for [e entries] (for [k h] (.getText (get e k))))) (vec->line h))
                              (m4p/inputfile->demand input-file)                              
                              (inputs-used-gui (root->outputdir root))
                              (.setVisible f false)) "Run"))
    (show-frame f main :x 600 :y 300)))

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
    (doall (map #(.setText ((first %) (first objs)) (second %)) [[:scenario "DemandGroup"] [:phase "Phase"] [:change "Change"]]))
    (doseq [o objs] (.add scenariopanel (:scenario o)) (.add phasepanel (:phase o)) (.add deltapanel (:change o)))
    (doseq [p panels] (.add mainpanel p) (.add frame mainpanel))
    (.add header (label (str "<html>" (data->str scenarios "Scenarios: ") "<br>" (str "<br>" (data->str phases "Phases: ")))))
    (.add mainpanel header)
    (.add mainpanel (fn->button (fn [] (let [lines (doall (for [o objs] [(.getText (:scenario o)) (.getText (:phase o)) 
                                                                         (let [t (.getText (:change o))] (try (c/read-num t) (catch Exception e t)))]))]
                                         (spit (str root "changes.txt") (reduce str (map #(str (first %) "\t" (second %) "\t" (last %) "\n") (filter #(not= "" (first %)) lines))))
                                         (ps/->output-new-phases file (ps/read-header file) (str root "changes.txt") file)
                                         (clojure.java.io/delete-file (str root "changes.txt"))
                                         (.setVisible frame false))) "Shift Phases"))   
    (show-frame frame mainpanel :x 432 :y 276)))

;;This gui will allow users to enter values to shift the start/duration of demand groups in a demand file
(defn extender-gui [filename]
  (let [f (frame :title "Extender")
        mainpanel (y-panel)
        panels (zipmap [:groupp :startp :durationp :startdelta :durationdelta] (repeat 5 (JPanel.)))
        times (ex/get-times filename "DemandGroup")
        objs (doall (for [i times :let [c (vec (zipmap (keys panels)
                                                 (flatten [(map #(label (str %)) ((juxt first second last) i)) (text-field )(text-field)])))]]
                      (do (doall (map #(.add ((first %) panels) (second %)) c))
                        (zipmap (map first c) (map second c)))))]
    (doseq [p (vals panels)] (.add mainpanel p))
    (.add mainpanel (fn->button #(do (ex/updates->file filename filename 
                                       (flatten (doall (for [o objs :let [num (fn [t] (if (not= "" t) (c/read-num t) 0))
                                                                          txt (fn [t] (.getText (t o)))]]
                                                         (map (fn [x] {:valkey (first x) :groupkey :DemandGroup 
                                                                       :key (txt :groupp) :delta (num (txt (second x)))})
                                                           [[:StartDay :startdelta] [:Duration :durationdelta]])))))
                                   (.setVisible f false)) "Shift Start Times"))
    (show-frame f mainpanel :x 276 :y 150)))

;;Buttons -> first is fn second is title
;;Set Working Dir, Set Existing Dir, Shift Phases, Shift DemandGroup, Sand Chart 
(defn ->buttons [lbl]
  (map #(fn->button (first %) (second %))
    [[#(let [root (file-choice->root (select-file))]
         (when (io/fexists? root)
           (.setText lbl (root->outputdir root)) (confirm-input-map root))) "Set Working Directory"]
     [#(let [f (choose-file)] (when (pos? (count (apply concat (vals (inputs-used (file->pdir f))))))
                                (.setText lbl (file->pdir f)))) "Set Existing Demand"]
     [#(phase-shifter-gui (.getText lbl)) "Shift Phases"]
     [#(extender-gui (outdir->demandfile (.getText lbl))) "Shift DemandGroup"]
     [#(c/demand-file->sand-charts (outdir->demandfile (.getText lbl)) :view true :save false) "Sand Chart"]]))

(defn main-gui []
  (let [f (frame :title "Demand Builder")
        p (y-panel)
        rdl (label "No working directory set")]
    (doseq [c (flatten (list rdl (->buttons rdl)))] (.add p c))
    (show-frame f p :x 350 :y 200)))

