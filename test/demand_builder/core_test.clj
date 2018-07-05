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

(defn get-output
  "Given a path to a taa_test_data directory, get the demand builder
  results."
  [path]
  (let [inpath (str path "/Input/")
        vignettes (find-file inpath vcons-file?)
        mapping (find-file inpath vmap-file?)]
    (f/root->demandfile inpath)
    (slurp (str inpath "Input_DEMAND.txt"))))

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

(def test-paths
  [(add-path "wrong_phase_extended")
   (add-path "simplest")
   (add-path "TestSet2")
   (add-path "TestCase3")
   (add-path "ManySRCs-1DemandGroup")
   (add-path "complexest")
   (add-path "inactive_extension")
   (add-path "intermittent")
   (add-path "increments")])

(deftest demandbuilder
  (doseq [p test-paths
          :let [out (get-output p)
                expected (get-expected p)
                ;;expected files don't always contain Strength.
                out (if (contains? (set (keys (first expected))) "Strength")
                      out
                      (map (fn [r] (dissoc r "Strength")) out))]]
    (println (str "Testing " p))
    (testing "Original output mispelled category, so let's make sure the fields match."
      (is (= (keys (dissoc (first out) "People")) (keys (first expected)))))
    (testing "If we compare sets, won't account for duplicates."
      (is (= (count out) (count expected))))
    (testing "Does the expected output match the output?"
      (is (= (set out) (set expected))))))
