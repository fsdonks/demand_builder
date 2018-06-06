(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f]]
            [spork.util.io])
  (:import [java.io File FileNotFoundException]
           [javax.swing JFrame JFileChooser JTextArea JPanel JLabel JButton JOptionPane]
           [java.awt.event ActionListener]))

(def ^:dynamic *closeon* 2)
(defn ->frame [& args] ;;Window for error message, first args is Title, second args is message
  (let [f (JFrame.) p (JPanel.) ta (JTextArea.)]
    (.setSize f 500 500) (.setDefaultCloseOperation f *closeon*)
    (.setRows ta 100) (.setSize ta 475 475) (.setLineWrap ta true)
    (.setTitle f (first args)) (.setText ta (second args))
    (.add p ta) (.add f p) (.setVisible f true) f))

(defn choose-file [& {:keys [title] :or {title ""}}]
  (let [f (javax.swing.JFrame.)
        c (javax.swing.JFileChooser.)]
    (.add f c)
    (.setDialogTitle c title)
    (.setFileSelectionMode c JFileChooser/DIRECTORIES_ONLY)
    (.setMultiSelectionEnabled c true)
    (let [x (.showOpenDialog c f)]
      (if  (zero? x)
        (map #(.getPath ^File %) (.getSelectedFiles c))
        (println "No file selected.")))))

;;Add an action listener to button that calls function func when pressed
(defn add-button-listener [button func]
  (.addActionListener button
    (proxy [ActionListener] []
      (actionPreformed [evt]
        (func)))))

(defn main-gui []
  (let [frame (JFrame. "Demand Builder")
        panel (JPanel.)
        rootLabel (JLabel. "No root directory set")
        selectButton (JButton. "Set Working Directory")
        buildButton (JButton. "Build Demand File")
        filesList (JLabel. "None")
        sandchartButton (JButton. "Sand Chart")]
    
    ;;Select working directory button listener
    (.addActionListener selectButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [root (str (first (choose-file :title "Select working directory")) "/")]
            (.setText rootLabel (if (nil? root) (.getText rootLabel) root))))))
    
    ;;Build Demand Button
    (.addActionListener buildButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (let [root (.getText rootLabel)
                filesUsed (when (not (nil? root))
                            (f/root->demandfile root))]
            (.setText filesList (str "<html>Demand File created using inputs:<br>" 
                                  (apply str (map #(str (spork.util.io/fname %) "<br>") filesUsed))))
            (.add panel filesList)
            (.setVisible frame false)
            (.setVisible frame true)))))
    
    ;;Sand Chart Button
    (.addActionListener sandchartButton
      (proxy [ActionListener] []
        (actionPerformed [evt]
          (c/demand-file->sand-charts (str (.getText rootLabel) (spork.util.io/fname (.getText rootLabel)) "_DEMAND.txt") :view true :save true))))
    
    (.add panel rootLabel)
    (.add panel selectButton)
    (.add panel buildButton)
    (.add panel sandchartButton)
    (.add frame panel)
    (.setSize frame 500 500)
    (.setDefaultCloseOperation frame *closeon*)
    (.setVisible frame true)))



(defn rf [] (require 'demand_builder.gui :reload))
