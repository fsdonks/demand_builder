(ns demand-builder.core-test
  (:require [clojure.test :refer :all]
            [demand_builder.core :refer :all]
            [spork.util.table :as tbl]))

;C-c C-k to load me and then switch to this namespace to run-tests

(defn get-output
  "Given a path to a taa_test_data directory, get the demand builder
  results."
  [path]
  (let [inpath (str path "/Input/")
        vignettes (find-file inpath vcons-file?)
        mapping (find-file inpath vmap-file?)]
    (->> (build-demand mapping vignettes inpath)
         ;durations are of number type to we'll stringify the whole thing
         (map (fn [r] (zipmap output-headers (map str r)))))))

(defn get-expected
  "Given a path to a taa_test_data directory, get the expected results in
  the same format as get-output."
  [path]
  (let [expath (str path "/Output/Input_DEMAND_expected.txt")]
    (->> (tbl/tabdelimited->records expath :keywordize-fields? false)
         (into [])
         (map (fn [r] (reduce-kv (fn [acc k v] (assoc acc k (str v))) {} r))))))

(defn compare-demands
  "given a path to a taa_test_data directory, check to see if the set of records from the demand builder match the expected outputs"
  [path]
  (clojure.set/difference (set (get-expected path)) (set (get-output path))))

(defn add-path [test-name]  (str "test/resources/" test-name))

(def test-paths
  [(add-path "wrong_phase_extended")])

(deftest demandbuilder
  (doseq [p test-paths
          :let [out (get-output p)
                expected (get-expected p)]]
    (println (str "Testing " p))
    (testing "Original output mispelled category, so let's make sure the fields match."
      (is (= (keys (first out)) (keys (first expected)))))
    (testing "If we compare sets, won't account for duplicates."
      (is (= (count out) (count expected))))
    (testing "Does the expected output match the output?"
      (is (= (set out) (set expected))))))
