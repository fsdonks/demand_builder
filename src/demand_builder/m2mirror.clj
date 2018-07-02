(ns demand_builder.m2mirror
  (:require [spork.util [io :as io]]
            [clojure.java [io :as jio]]))

;;Sha1 hash function for byte array
(defn sha1 [bytes]
  (map #(read-string (str %)) (.digest (java.security.MessageDigest/getInstance "sha1") bytes)))

;;Creates byte array for file path
(defn file-to-byte-array [file]
  (let [result (byte-array (.length (java.io.File. file)))]
    (with-open [in (java.io.DataInputStream. (clojure.java.io/input-stream file))]
      (.readFully in result))
    result))

;;Converts int to hex
(defn as-hex [i]
  (let [hex (apply str (take-last 2 (format "%x" i)))]
    (if (= 1 (count hex)) (str "0" hex) hex)))

;;Gets tha sha1 hash of the contents of a file
(defn sha1-file [file]
  (let [sha1-hash (apply str (map as-hex (sha1 (file-to-byte-array file))))]
    (spit (str file ".sha1") sha1-hash)
    sha1-hash))    

;;List all files and sub folders
(defn list-all-files-recursive [root]
  (let [paths (io/list-files root)
        files (filter #(not (.isDirectory %)) paths)
        dirs (filter #(.isDirectory %) paths)]
    (flatten 
      (conj files
       (for [d dirs]
         (list-all-files-recursive d))))))

;;Gets file extention 
(defn file-type [file] 
  (last (clojure.string/split file #"[.]")))

;;Checks for file extention 
(defn is-filetype? [file type]
  (= (file-type file) type))

;;Hashes all files in directory (all jar or pom files)
(defn sha1-all-repos [root]
  (let [files (filter #(or (is-filetype? % "jar") (is-filetype? % "pom")) (map str (list-all-files-recursive root)))]
    (println (count (into [] (pmap #(sha1-file %) files))) "files hashed")))

;;Checks if the new-file exist and if it does, is it older then the current file
(defn move? [file new-file]
  (not (and (.exists (java.io.File. new-file)) (< (.lastModified (java.io.File. file)) (.lastModified (java.io.File. new-file))))))

;;Coppies file when either the local file is new or the existing file is older than the local file
(defn mirror-file [file new-file]
  ;;Only copy when the file is new or the current file is newer than the existing file (file in new-dir is older)
  (when (move? file new-file) 
    (io/deep-copy file new-file)))

;;Coppies all files from dir to new dir (deep copy)
(defn mirror-dir [dir new-dir]
  (let [slash (fn [s] (clojure.string/replace s #"\\" "/"))]
    ;;Have to switch backslash to forward slash to turn path name into regex
    (let [new-file (fn [f] (clojure.string/replace  (slash f) (re-pattern (slash dir)) (slash new-dir)))
          files (map str (list-all-files-recursive dir))]
      (println (count (into [] (pmap #(mirror-file % (new-file %)) files))) "files coppied"))))

;;Creates any missing sha1 hashes for repos and mirrors them to remote location 
;;m2-local is where the files currently exist
;;m2-remote is where the files should be coppied to
(defn mirror-m2 [m2-local m2-remote]
  (sha1-all-repos m2-local)
  (mirror-dir m2-local m2-remote))

