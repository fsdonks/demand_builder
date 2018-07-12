(ns demand-builder.core-test
  (:require [clojure.test :refer :all]
            [demand_builder.core :refer :all]
            [demand_builder [formatter :as f]]
            [spork.util.table :as tbl]))

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

(defn add-path [test-name]  (str "test/resources/updated-test/" test-name))

(def test-paths
  [(add-path "wrong_phase_extended")
   (add-path "simplest")
   (add-path "TestSet2")
   (add-path "TestCase3")
   (add-path "ManySRCs-1DemandGroup")
   (add-path "complexest")
   (add-path "inactive_extension")
   (add-path "intermittent")
   (add-path "increments")
   (add-path "duplicates")
   (add-path "early_map")])

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
