(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f] [extend :as ex] [phaseshifter :as ps] m4plugin]
            [spork.util [io :as io]]
            [seesaw core])
  (:import [java.io File FileNotFoundException]
           [javax.swing BoxLayout JFrame JFileChooser JTextArea JPanel JLabel JButton JOptionPane JTextField]
           [java.awt.event ActionListener])
  (:use [seesaw core chooser]))

(native!)
(defn select-file []
  (seesaw.chooser/choose-file :type :open :selection-mode :files-only
    :success-fn (fn [fc file] (.getAbsolutePath file))))

(defn map->inputfile [m]
 (let [p (:Path (first m))
       out (clojure.string/replace p (io/fname p) "input-map.txt")]
   (spork.util.stream/records->file m out)
   out))  

(defn gui->inputfile [p]
  (let [c (filter #(instance? javax.swing.JLabel %) (.getComponents p))
        vals (map #(.getText %) c)]
    (for [v vals] (zipmap [:Path :Type :ForceCode :Sheetname] (clojure.string/split v #"<>")))))

(defn new-row-gui [main main-panel]
  (let [f (frame :title "Add file")
        _ (.setVisible main false)
        p (javax.swing.JPanel.)
        _ (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
        add (fn [content] (.add p content))
        file (label (str (select-file)))
        opts (listbox :model ["MAP" "CONSOLIDATED" "FORGE"])
        fc (text "SE-???")
        sheet (text "Sheetname")
        submit (button :text "Add file")
        content [file opts fc sheet submit]]
    (listen submit :action  (fn [e] 
                              (.setVisible f false)
                              (.add main-panel (label (str (text file) "<>" (selection opts) "<>" (text fc) "<>" (text sheet))))
                              (.setVisible main true)))
    (doseq [c content] (add c))
    (.add f p)
    (-> f pack! show!)))

(defn demand-gui []
  (let [f (frame :title "Demand Builder")
        p (javax.swing.JPanel.)
        add-file (button :text "Add file")
        run-button (button :text "Run demand builder")]
   (listen add-file :action (fn [e] (new-row-gui f p)))
   (listen run-button :action (fn [e] (demand_builder.m4plugin/inputfile->demand (map->inputfile (gui->inputfile p)))))
   (.add p add-file)
   (.add p run-button)
   (.add f p)
   (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
   (-> f pack! show!)))



(comment
  (def ^:dynamic *closeon* 2)
  
  (defn choose-file [& {:keys [title] :or {title ""}}]
    (let [f (javax.swing.JFrame.)
          c (javax.swing.JFileChooser.)]
      (.add f c)
      (.setDialogTitle c title)
      (.setFileSelectionMode c JFileChooser/DIRECTORIES_ONLY)
      (.setMultiSelectionEnabled c true)
      (let [x (.showOpenDialog c f)]
        (if  (zero? x)
          (map #(.getPath ^File %) (.getSelectedFiles c))))))
  
  ;;Will add a listener to a button that when pressed will calls function fn (fn cannot have any arguments)
  (defn add-button-listener [button fn]
    (.addActionListener button
      (proxy [ActionListener] []
        (actionPerformed [evt] (fn)))))
  
  ;;Will create a GUI for the phase header and adjust the demand file according to user input.
  (defn phase-shifter-gui [root]
    (let [file (str root (spork.util.io/fname root) "_DEMAND.txt")
          data (c/file->map-list file)
          scenarios (filter #(clojure.string/includes? % "SE") (distinct (map :DemandGroup data)))
          phases (filter #(clojure.string/includes? % "PH") (distinct (map :Operation data)))
          scenariostr (str "Scenarios:<br>\t" (apply str (map #(str "\t" % "<br>") scenarios)))
          phasestr (str "Phases:<br>\t" (apply str (map #(str % "\t") phases)))
          frame (JFrame. "Phase Shifter")
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
      (add-button-listener button
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
  
  ;;Checks if dir exists and contains files needed to create Demand Files
  (defn valid-dir? [dir]
    (let [files (spork.util.io/list-files (spork.util.io/as-directory dir))
          filenames (map #(spork.util.io/fname %) files)
          has-type (fn [pref] (pos? (count (filter #(clojure.string/includes? % pref) filenames))))
          has-map? (has-type "MAP_")
          has-cons? (has-type "CONSOLIDATED_")]
      (and has-map? has-cons?)))
  
  ;;Creates a button with a listener that will open a file select menu and change label to dir only when it contains valid inputs.
  (defn select-root-button [button label]
    (add-button-listener button
      (fn [] (let [root (spork.util.io/as-directory (first (choose-file :title "Select working directory")))]
               (if (valid-dir? root) (.setText label root) (.setText label (str "No valid inputs found in " root))))))
    button)  
  
  ;;Creates a button that will run demand builder using root specified in label
  (defn build-demand-button [button root-label files-label]
    (add-button-listener button
      (fn [] (let [root (.getText root-label)
                   files (try
                           (f/root->demandfile root)
                           (catch Exception e
                             (do
                               (.setText files-label
                                 (str "<html>Could not build Demand File from inputs at root: " root "<br>" (.getMessage e) "<br>"))
                               (valid-dir? root))))]
               (if files
                 (try
                   (.setText files-label (str "<html>Demand File creates using inputs:<br>" (apply str (map #(str (spork.util.io/fname %) "<br>") files))))
                   (catch Exception e)) ;;Do nothing, already set error message, ignore.
                 (.setText files-label (str "<html>Could not find valid inputs.<br>" root))))))
    button)
  
  ;;Creats button that will call sand chart creator. Will prompt for supply file when needed
  (defn sand-chart-button [button root-label files-label]
    (add-button-listener button
      (fn []
        (let [file (str (.getText root-label) (spork.util.io/fname (.getText root-label)) "_DEMAND.txt")]
          (if (spork.util.io/fexists? file)
           (try
             (c/demand-file->sand-charts file :view true :save false)
             (catch java.lang.AssertionError e 
               (c/demand-file->sand-charts file :supplyfile (first (choose-file :title "Supply file to look up Strength per SRC")) :view true :save false))
             (catch Exception e)) ;;Ignore other exceptions
           (.setText files-label (str "<html>Could not create Sand Chart from inputs at root: " (.getText root-label) "<br></html>"))))))
    button)
  
  ;;Creates a button that will open a new gui that can reformat the start/duration times of an existing Demand File.
  (defn shift-button [button label]
    (add-button-listener button
      (fn [] 
        (try
          (let [root (str (.getText label) (spork.util.io/fname (.getText label)) "_DEMAND.txt")]
           (extender-gui root))
          (catch Exception e)))) ;;ignore errors (do nothing if needed values not yet set)
    button)
  
  (defn phase-shift-button [button label]
    (add-button-listener button 
      (fn [] (try 
               (phase-shifter-gui (.getText label))
               (catch Exception e)))) ;;phase shifter handles errors already 
    button) 
  
  (defn main-gui []
    (let [frame (JFrame. "Demand Builder")
          panel (JPanel.)
          root-label (JLabel. "No working directory set")
          files-list (JLabel. "<html>No files selected<br></html>")
          select-button (select-root-button (JButton. "Set Working Directory") root-label)
          build-button (build-demand-button (JButton. "Build Demand File") root-label files-list)
          sandchart-button (sand-chart-button (JButton. "Sand Chart") root-label files-list)
          shift-button (shift-button (JButton. "Shift Times") root-label)
          phase-shift-button (phase-shift-button (JButton. "Shift Phases") root-label)]
      (.add panel root-label)
      (.add panel select-button)
      (.add panel build-button)
      (.add panel files-list)
      (.add panel shift-button)
      (.add panel phase-shift-button)
      (.add panel sandchart-button)
      (.setLayout panel (javax.swing.BoxLayout. panel javax.swing.BoxLayout/Y_AXIS))
      (.add frame panel)
      (.setSize frame 494 310)
      (.setDefaultCloseOperation frame *closeon*)
      (.setVisible frame true)
      frame))
  
  (defn rf [] (require 'demand_builder.gui :reload)))
