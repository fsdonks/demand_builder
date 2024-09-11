;; ============================================================================
(ns demand_builder.core
  (:gen-class)
  (:require [spork.util [io :as io]]
            [demand_builder [gui :as guins]]))

;; ============================================================================
;; ===== FUNCTIONS TO AUTOMATE MAKING DEMAND FILES GIVEN THE ROOT DIR =========
;; ===========================================================================
;; Returns true when file is filetype 
(defn is-filetype? [filename filetype]
  (= 2 (count (clojure.string/split filename (re-pattern filetype)))))

(defn vcons-file? [filename] (is-filetype? filename "CONSOLIDATED_"))
(defn vmap-file? [filename] (is-filetype? filename "MAP_"))
(defn forge-file? [filename] (is-filetype? filename "FORGE_"))

;; Gets filenames of filetype from root dir 
(defn root->filetype [root fn-type]
  (filter fn-type (map io/fname (io/list-files root))))

(defn find-file [root f]
  "returns the path of file from root when filepaths are filtered by f"
  (str root "\\" (first (root->filetype root f))))

(comment
  "This was the previous gui entry point.  If a GUI option is desired
  in the future, this should be reworked so that it fits with the taa
  preprocess-taa workflow."
(defn gui [& {:keys [exit?] :or {exit? true}}]
  (guins/main-gui :exit exit?)))
