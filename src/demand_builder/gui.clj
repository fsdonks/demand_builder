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
                                      (.setVisible f false))))
    (.add main submit)
    (.setLayout main (javax.swing.BoxLayout. main javax.swing.BoxLayout/Y_AXIS))
    (.add f main)
    (.setSize f 600 300)
    (.setVisible f true)))

;;User selects root directory using file select menu
;;Demand Builder automatically tries to match the files;
;;Attempts to find the Map and Conslidated files, 
;;For each FORGE, attempts to match FORGE file to ForceCode (from MAP)
;;The user is then given a form listing the matchings and is asked to confirm or change the mapping
;;Pressing run will run the formatter and generate the new demand file
(defn demand-builder-main-gui []
  (let [root (select-file)]
    (when (io/fexists? root)
      (if (.isDirectory (io/file root)) ;;Can select the directory iteself, or any file in root
        (confirm-input-map root)
        (confirm-input-map (io/fdir root))))))

