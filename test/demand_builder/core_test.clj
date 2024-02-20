(ns demand-builder.core-test
  (:require [clojure.test :refer :all]
            [demand_builder.core :refer :all]
            [demand_builder [formatter :as f]]
            [spork.util.table :as tbl]
            [clojure.string :as strlib]
            [clojure.java [io :as io]]))

;C-c C-k to load me and then switch to this namespace to run-tests

(defn run-tests-nout
  "When you don't want to have the expected and actual outputs printed to the REPL, you can use this instead of run-tests"
  []
  (defmethod report :fail [m]
    (with-test-out
      (inc-report-counter :fail)
      (println "\nFAIL in" (testing-vars-str m))
      (when (seq *testing-contexts*) (println (testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected: something else")
      (println "  actual: not else")))
  (let [res (run-tests)]
    (defmethod report :fail [m]
      (with-test-out
        (inc-report-counter :fail)
        (println "\nFAIL in" (testing-vars-str m))
        (when (seq *testing-contexts*) (println (testing-contexts-str)))
        (when-let [message (:message m)] (println message))
        (println "expected:" (pr-str (:expected m)))
        (println "  actual:" (pr-str (:actual m)))))
    res))

(def demand-schema {:Type :text
                    :Enabled :text
                    :Priority :text
                    :Quantity :text
                    :DemandIndex :text
                    :StartDay :text
                    :Duration :text
                    :Overlap :text
                    :SRC :text
                    :SourceFirst :text
                    :DemandGroup :text
                    :Vignette :text
                    :Operation :text
                    :Category :text
                    :Title10_32 :text
                    :OITitle :text
                    :Strength :text})

(defn get-output
  "Given a path to a taa_test_data directory, get the demand builder
  results."
  [path]
  (let [inpath (str path "/Input/")
        vignettes (find-file inpath vcons-file?)
        mapping (find-file inpath vmap-file?)]
    (f/root->demandfile inpath)
    (into [] (spork.util.table/tabdelimited->records (str inpath "Input_DEMAND.txt") :schema demand-schema :keywordize-fields? false))))
    

(defn get-expected
  "Given a path to a taa_test_data directory, get the expected results in
  the same format as get-output."
  [path]
  (let [expath (str path "/Output/Input_DEMAND_expected.txt")]
    (->> (tbl/tabdelimited->records expath :keywordize-fields? false)
         (into [])
         (map (fn [r] (reduce-kv (fn [acc k v] (assoc acc k (str v))) {} r))))))

;;changed to a stronger (but less informative) equality check.  Implement your
;;own diffing routine if need be.  This will just check the hash of each record set,
;;which in turn hashes the records.  Hashes should be equal.
(defn compare-demands
  "given a path to a taa_test_data directory, check to see if the set of records from the demand builder match the expected outputs"
  [path]
  (= (set (get-expected path)) (set (get-output path))))

(defn add-path [test-name]  (str "test/resources/" test-name))

(defn last-split
  "Split a string based on another string and return the last split.
  Used in deep-copy."
  [in-str sep]
  (last (strlib/split in-str (re-pattern sep))))

(defn forge-filt [io-file]
  (not= (.getName io-file) "FORGE_SE-99.txt"))

(defn deep-copy 
	"Copies all files in sourcedir to targetdir.  Creates folders
  as needed.  Filter java.io.File objects with filt"
  [sourcedir targetdir & {:keys [filt] :or {filt (fn [f] true)}}]
  (let [source-paths (->> (io/file sourcedir)
                          (file-seq)
                          (filter filt)
                          (map #(.getPath %)))
        ;;Want the dest-paths to include everything but the sourcedir.
	dest-paths   (map #(str targetdir (last-split % sourcedir))
                          ;;First source-path will be the sourcedir
                          (rest source-paths))              
	pathmap (zipmap (rest source-paths) dest-paths)]
	  (doseq [kv pathmap]
		(let [[kf vf] (map io/file kv)]		
			(when (not (.exists vf)) (io/make-parents vf))
			(when (.isFile kf) (io/copy kf vf))))))

;;(deep-copy "test/resources/updated-test/"
;;  "/test/resources/"
;; :filt forge-filt)
;;(doseq [p test-names] (get-output (add-path p)))

(def test-names
  [ "wrong_phase_extended"
   "simplest"
   "TestSet2"
   "TestCase3"
   "ManySRCs-1DemandGroup"
   "complexest"
   "inactive_extension"
   "intermittent"
   "increments"
   "duplicates"
   "early_map"
   "earlier_map"
   "forge_decimals"
   ])

(def test-paths
  (mapcat (fn [test-name]           
            [;;SRC_by_Day tests
             ;;(all test data same as unit node details tests except
             ;;for the FORGE files.
             (add-path test-name)
             ;;Unit Node Details tests
             (add-path (str test-name "/updated-test"))]) test-names))           
  
(deftest demandbuilder
  (doseq [p test-paths
          :let [_ (println (str "Testing " p))
                out (get-output p)
                expected (get-expected p)
                ;;expected files don't always contain Strength.
                out (if (contains? (set (keys (first expected))) "Strength")
                      out
                      (map (fn [r] (dissoc r "Strength")) out))]]
    (testing "Original output mispelled category, so let's make sure the fields match."
      (is (= (sort (filter #(not (or (= "People" %) (= "Strength" %))) (keys (first out)))) 
             (sort (filter #(not= "Strength" %) (keys (first expected)))))))
    (testing "If we compare sets, won't account for duplicates."
      (is (= (count out) (count expected))))
    (testing "Does the expected output match the output?"
      (is (= (set (map #(dissoc % "Strength") out))
             (set (map #(dissoc % "Strength") expected)))))))
