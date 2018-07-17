(ns demand_builder.gui
  (:require [demand_builder [chart :as c] [formatter :as f] [extend :as ex] [phaseshifter :as ps] [m4plugin :refer :all]]
            [spork.util [io :as io] [table :as tbl]]
            [spork.util.excel [core :refer :all] [docjure :refer :all]]
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

(def del " : ")
(defn gui->inputfile [p]
  (let [c (filter #(instance? javax.swing.JLabel %) (.getComponents p))
        vals (map #(.getText %) c)]
    (for [v vals] (zipmap [:Path :Type :ForceCode :Sheetname] (clojure.string/split v (re-pattern del))))))

(defn new-row-gui [main main-panel]
  (let [f (frame :title "Add file")
        _ (.setVisible main false)
        p (javax.swing.JPanel.)
        _ (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
        add (fn [content] (.add p content))
        file (label (str (select-file)))
        opts (label (most-likely-file (.getText file)));;(listbox :model ["MAP" "CONSOLIDATED" "FORGE"])
        fc (if (= "FORGE" (.getText opts)) (text "SE-???") (label ""))
        sheet (let [sheets (list-sheets (.getText file))]
                (if (= 1 (count sheets))
                  (label (first sheets))
                  (if (= "FORGE" (.getText opts))
                    (if (first (filter #(= "Unit_Node_Detail" %) sheets))
                      (label "Unit_Node_Detail")
                      (if (first (filter #(= "SRC_By_Day" %) sheets))
                        (label "SRC_By_Day")
                        (map #(str % " : ") sheets))))))        
        submit (button :text "Add file")
        content [file opts fc sheet submit]]
    (defn submit-file [] 
      (.setVisible f false)
      (.add main-panel (label (str (text file) del (text opts) del (text fc) del (text sheet))))
      (.setVisible main true))
    (listen submit :action  (fn [e] (submit-file)))
    (doseq [c content] (add c))
    (.add f p)
    (-> f pack! show!)
    (if (not= "FORGE" (.getText opts)) (submit-file))))

(defn demand-gui []
  (let [f (frame :title "Demand Builder")
        p (javax.swing.JPanel.)
        add-file (button :text "Add file")
        run-button (button :text "Run demand builder")]
    (listen add-file :action (fn [e] (new-row-gui f p)))
    (listen run-button :action (fn [e] 
                                 (println  
                                   (demand_builder.m4plugin/inputfile->demand (map->inputfile (gui->inputfile p)))) 
                                 (.setVisible f false)))
    (.add p add-file)
    (.add p run-button)
    (.add f p)
    (.setLayout p (javax.swing.BoxLayout. p javax.swing.BoxLayout/Y_AXIS))
    (.setSize (-> f pack! show!) 774 188)))

