(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f]]
            [spork.util.io])
  (:import [java.io File FileNotFoundException]
           [javax.swing BoxLayout JFrame JFileChooser JTextArea JPanel JLabel JButton JOptionPane]
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

(defn main-gui []
  (let [frame (JFrame. "Demand Builder")
        panel (JPanel.)
        rootLabel (JLabel. "No working directory set")
        selectButton (JButton. "Set Working Directory")
        buildButton (JButton. "Build Demand File")
        filesList (JLabel. "<html>No files selected<br></html>")
        sandchartButton (JButton. "Sand Chart")]

    ;;Select working directory button listener
    (.addActionListener selectButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [root (str (first (choose-file :title "Select working directory")) "/")]
            (.setText rootLabel (if (= "/" root) (.getText rootLabel) root))))))
    
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
                                    (apply str (map #(str (spork.util.io/fname %) "<br>") filesUsed))))
              (.setText filesList (str "<html>Could not build Demand File from inputs at root: " root"</html>")))))))
    
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
    ;(.setBackground panel (java.awt.Color/blue))
    (.setLayout panel (javax.swing.BoxLayout. panel javax.swing.BoxLayout/Y_AXIS))
    (.add frame panel)
    (.setSize frame 494 210)
    (.setDefaultCloseOperation frame *closeon*)
    (.setVisible frame true)
    frame))

(defn rf [] (require 'demand_builder.gui :reload))
