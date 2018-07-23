(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f] [extend :as ex] [phaseshifter :as ps] [m4plugin :as m4p]]
            [spork.util [io :as io] [table :as tbl]]
            [seesaw core]
            [clojure.java [io :as jio]])
  (:import [java.io File FileNotFoundException]
           [javax.swing BoxLayout JFrame JFileChooser JTextArea JPanel JLabel JButton JOptionPane JTextField]
           [java.awt.event ActionListener])
  (:use [seesaw core chooser]))

(native!)
(defn select-file []
  (seesaw.chooser/choose-file :type :open :selection-mode :files-and-dirs
    :success-fn (fn [fc file] (.getAbsolutePath file))))

(defn map->inputfile [m]
 (let [p (:Path (first m))
       out (clojure.string/replace p (io/fname p) "input-map.txt")]
   (spork.util.stream/records->file m out)
   out))

(defn file-type [root type]
  (filter #(clojure.string/includes? % type) (map str (io/list-files root))))

(defn inputs-used [root]
  (let [mapfile (first (file-type root "MAP_"))
        confile (first (file-type root "CONSOLIDATED_"))
        forges (file-type root "FORGE_")]
    {:MAP mapfile :CONS confile :FORGE forges}))

(defn y-panel []
  (let [p (javax.swing.JPanel.)]
    (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
    p))

(defn inputs-used-gui [root]
  (let [files (inputs-used root)
        f (frame :title "Inputs Used")
        p (y-panel)]
    (.add p (label (io/fname (:MAP files))))
    (.add p (label (io/fname (:CONS files))))
    (doseq [forge (:FORGE files)]
      (.add p (label (io/fname forge))))
    (.add f p)
    (.setSize f 350 100)
    (.setVisible f true)))

;;Builds a form gui showing the file mappings that will be used to build demand file
(defn confirm-input-map [root]
  (m4p/root->inputmap root) ;;build the input map from root
  (let [input-file (str (io/as-directory root) "input-map.txt")
        h ["Path" "Type" "ForceCode" "Sheetname"]
        data (into [] (tbl/tabdelimited->records input-file :keywordize-fields? false))
        f (frame :title "Confirm Demand Builder Inputs")
        submit (button :text "Run")
        main (javax.swing.JPanel.)
        entries (into [] (for [d data :let [p (javax.swing.JPanel.)]]
                           (do 
                             (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
                             (.add main p)
                             (let [path (label (get d "Path"))
                                   type (text (get d "Type"))
                                   fc (text (get d "ForceCode"))
                                   sheet (text (get d "Sheetname"))]
                               (.add p path) (.add p type) (.add p fc) (.add p sheet)
                               {"Path" path "Type" type "ForceCode" fc "Sheetname" sheet}))))]
    (listen submit :action  (fn [e] (let [lines (for [e entries]
                                                  (for [k h] (.getText (get e k))))
                                          ->line (fn [x] (str (apply str (map #(str % "\t") x)) "\n"))]
                                      (if (io/fexists? input-file)
                                        (jio/delete-file input-file))
                                      (spit input-file (->line h))
                                      (doseq [l lines]
                                        (spit input-file (->line l) :append true))
                                      (m4p/inputfile->demand input-file)
                                      (inputs-used-gui (str (io/as-directory root) (io/as-directory m4p/outputdir)))
                                      (.setVisible f false))))
    (.add main submit)
    (.setLayout main (javax.swing.BoxLayout. main javax.swing.BoxLayout/Y_AXIS))
    (.add f main)
    (.setSize f 600 300)
    (.setVisible f true)))
    

(defn file-choice->root [f]
  (when f (if (.isDirectory (io/file f)) f (io/fdir f))))

(def ^:dynamic *closeon* 2)


;;Will create a GUI for the phase header and adjust the demand file according to user input.
(defn phase-shifter-gui [root]
  (let [file (str root (spork.util.io/fname root) "_DEMAND.txt")
        data (c/file->map-list file)
        scenarios (filter #(clojure.string/includes? % "SE") (distinct (map :DemandGroup data)))
        phases (filter #(clojure.string/includes? % "PH") (distinct (map :Operation data)))
        scenariostr (str "Scenarios:<br>\t" (apply str (map #(str "\t" % "<br>") scenarios)))
        phasestr (str "Phases:<br>\t" (apply str (map #(str % "\t") phases)))
        frame (JFrame. "Phase Shifter")
        ;_ (.setIconImage frame (javax.imageio.ImageIO/read (io/file "./phase-shifter-icon.png")))
        mainpanel (JPanel.)
        scenariopanel (JPanel.)
        phasepanel (JPanel.)
        deltapanel (JPanel.)
        headerlabel (JLabel. (str "<html>" scenariostr "<br>" phasestr))
        objs (into [] (for [i (range 10) :let [scenario (JTextField.) phase (JTextField.) change (JTextField.)]]
                        (do (.setColumns scenario 10) (.setColumns phase 7) (.setColumns change 7)
                          {:scenario scenario :phase phase :change change})))
        panels [scenariopanel phasepanel deltapanel]
        header (JPanel.)
        button (JButton. "Shift Phases")
        demandfile (str root (spork.util.io/fname root) "_DEMAND.txt")]
    (.setText (:scenario (first objs)) "DemandGroup")
    (.setText (:phase (first objs)) "Phase")
    (.setText (:change (first objs)) "Change")
    (doseq [o objs] (.add scenariopanel (:scenario o)) (.add phasepanel (:phase o)) (.add deltapanel (:change o)))
    (.add header headerlabel)
    (.setLayout header (BoxLayout. header BoxLayout/Y_AXIS))
    (.add mainpanel header)
    (doseq [p panels]
      (.setLayout p (BoxLayout. p BoxLayout/Y_AXIS))
      (.add mainpanel p)
      (.add frame mainpanel))
    (.setSize frame 432 276)
    (.setVisible frame true)
    (listen button :action
      (fn []
        (let [lines (into [] 
                      (for [o objs] 
                        (vector 
                          (.getText (:scenario o)) 
                          (.getText (:phase o)) 
                          (try (c/read-num (.getText (:change o))) 
                            (catch Exception e (.getText (:change o)))))))
              change-file (str root "changes.txt")
              change-output (reduce str (map #(str (first %) "\t" (second %) "\t" (last %) "\n") (filter #(not= "" (first %)) lines)))]

          
          (spit change-file change-output)
          (ps/->output-new-phases demandfile (ps/read-header demandfile) (str root "changes.txt") demandfile)
          (clojure.java.io/delete-file (str root "changes.txt"))
          (.setVisible frame false))))
    (.add mainpanel button)))


;;This gui will allow users to enter values to shift the start/duration of demand groups in a demand file
(defn extender-gui [filename]
  (let [frame (JFrame. "Extender")
        ;_ (.setIconImage frame (javax.imageio.ImageIO/read (io/file "./extender-icon.png")))
        mainpanel (JPanel.) grouppanel (JPanel.) startpanel (JPanel.) startdelta (JPanel.) durationpanel (JPanel.) durationdelta (JPanel.)
        shiftButton (JButton. "Shift Start Times")
        panels [grouppanel startpanel startdelta durationpanel durationdelta]
        times (ex/get-times filename "DemandGroup")
        objs (into []
               (for [i times :let [group (JLabel. (str (first i)))
                                   start (JLabel. (str (second i)))
                                   duration (JLabel. (str (last i)))
                                   sd (JTextField.) 
                                   dd (JTextField.)]]
                (do
                  (.setColumns sd 7)
                  (.setColumns dd 7)
                  (.add grouppanel group)
                  (.add startpanel start)
                  (.add durationpanel duration)
                  (.add startdelta sd)
                  (.add durationdelta dd)
                  {:group group :start start :duration duration :sd sd :dd dd})))]
 
    (.addActionListener shiftButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [shifts (flatten (into []
                                  (for [o objs]
                                    [{:valkey :StartDay :groupkey :DemandGroup :key (.getText (:group o)) 
                                      :delta (if (not= "" (.getText (:sd o))) (c/read-num (.getText (:sd o))) 0)}
                                     {:valkey :Duration :groupkey :DemandGroup :key (.getText (:group o)) 
                                      :delta (if (not= "" (.getText (:dd o))) (c/read-num (.getText (:dd o))) 0)}])))]
            (ex/updates->file filename filename shifts)
            (.setVisible frame false)))))
    (doseq [p panels]
      (.setLayout p (BoxLayout. p BoxLayout/Y_AXIS))
      (.add mainpanel p)) 
    (.add mainpanel shiftButton)
    (.add frame mainpanel)
    (.setSize frame 300 150)
    (.setVisible frame true)
    objs))

(defn select-root-button [root-dir-label]
  (let [b (button :text "Select Working Directory")]
    (listen b :action (fn [e] (let [root (file-choice->root (select-file))]
                                (when (io/fexists? root)
                                  (.setText root-dir-label (str (io/as-directory root) (io/as-directory m4p/outputdir)))
                                  (confirm-input-map root)))))
    b))

(defn phase-shifter-button [root-dir-label]
  (let [b (button :text "Shift Phases")]
    (listen b :action (fn [e] (phase-shifter-gui (.getText root-dir-label))))
    b))

(defn extender-button [root-dir-label]
  (let [b (button :text "Shift Times")]
    (listen b :action (fn [e] (extender-gui
                                (str (io/as-directory (.getText root-dir-label)) (io/fname (.getText root-dir-label)) "_DEMAND.txt"))))
    b))

(defn sand-chart-button [root-dir-label]
  (let [b (button :text "Sand Chart")]
    (listen b :action (fn [e] 
                        (let [root (.getText root-dir-label)]
                          (c/demand-file->sand-charts (str (io/as-directory root) (io/fname root) "_DEMAND.txt")
                            :view true :save false))))
    b))

(defn show-frame [f p & {:keys [x y] :or {x 100 y 100}}]
  (.add f p)
  (.setSize f x y)
  ;(.setIconImage f (javax.imageio.ImageIO/read (io/file "./icon.png")))
  (.setVisible f true))

(defn main-gui []
  (let [f (frame :title "Demand Builder")
        p (y-panel)
        rdl (label "No working directory set")]
    (doseq [c (flatten (list rdl (map #(% rdl) [select-root-button phase-shifter-button extender-button sand-chart-button])))]
      (.add p c))
    (show-frame f p :x 350 :y 200)))


