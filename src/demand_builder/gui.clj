(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f] [extend :as ex]]
            [spork.util.io])
  (:import [java.io File FileNotFoundException]
           [javax.swing BoxLayout JFrame JFileChooser JTextArea JPanel JLabel JButton JOptionPane JTextField]
           [java.awt.event ActionListener]))

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

;;This gui will allow users to enter values to shift the start/duration of demand groups in a demand file
(defn extender-gui [filename]
  (let [frame (JFrame. "Extender")
        mainpanel (JPanel.) grouppanel (JPanel.) startpanel (JPanel.) startdelta (JPanel.) durationpanel (JPanel.) durationdelta (JPanel.)
        shiftButton (JButton. "SHIFT")
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

(defn main-gui []
  (let [frame (JFrame. "Demand Builder")
        panel (JPanel.)
        rootLabel (JLabel. "No working directory set")
        selectButton (JButton. "Set Working Directory")
        buildButton (JButton. "Build Demand File")
        filesList (JLabel. "<html>No files selected<br></html>")
        sandchartButton (JButton. "Sand Chart")
        shiftButton (JButton. "Shift Times")]

    ;;Select working directory button listener
    (.addActionListener selectButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [root (str (first (choose-file :title "Select working directory")) "/")]
            (.setText rootLabel (if (= "/" root) (.getText rootLabel) root))))))
    
    ;;Shift Button
    (.addActionListener shiftButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [root (str (.getText rootLabel) (spork.util.io/fname (.getText rootLabel)) "_DEMAND.txt")]
            (extender-gui root)))))
    
    ;;Build Demand Button
    (.addActionListener buildButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [root (.getText rootLabel)
                filesUsed (when (not= "No root directory set" root)
                            (try
                              (f/root->demandfile root)
                              (catch Exception e 
                                (do
                                  (.setText filesList (str "<html>Could not build Demand File from inputs at root: " root "<br>" (.getMessage e) "<br></html>"))))))]
            (if (not= nil filesUsed)
              (.setText filesList (str "<html>Demand File created using inputs:<br>" 
                                    (apply str (map #(str (spork.util.io/fname %) "<br>") filesUsed)))))))))
    ;(.setText filesList (str "<html>Could not build Demand File from inputs at root: " root"</html>")))))))
    
    ;;Sand Chart Button
    (.addActionListener sandchartButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (try
            (c/demand-file->sand-charts (str (.getText rootLabel) (spork.util.io/fname (.getText rootLabel)) "_DEMAND.txt") :view true :save true)
            (catch java.lang.AssertionError e ;;If no people/strength in demand file, ask for supply file to look up strength by src
              (c/demand-file->sand-charts (str (.getText rootLabel) (spork.util.io/fname (.getText rootLabel)) "_DEMAND.txt")
                :supplyfile (first (choose-file :title "Supply file to look up Strength per SRC")) :veiw true :save true))
            (catch Exception e (.setText filesList (str "<html>Could not create Sand Chart from inputs at root: " (.getText rootLabel) "<br></html>")))))))
    (.add panel rootLabel)
    (.add panel selectButton)
    (.add panel buildButton)
    (.add panel filesList)
    (.add panel sandchartButton)
    (.add panel shiftButton)
    ;(.setBackground panel (java.awt.Color/blue))
    (.setLayout panel (javax.swing.BoxLayout. panel javax.swing.BoxLayout/Y_AXIS))
    (.add frame panel)
    (.setSize frame 494 210)
    (.setDefaultCloseOperation frame *closeon*)
    (.setVisible frame true)
    frame))



(defn rf [] (require 'demand_builder.gui :reload))
