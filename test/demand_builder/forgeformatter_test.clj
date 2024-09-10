(ns demand-builder.forgeformatter-test
  (:require [clojure.test :refer :all]
            [demand_builder [forgeformatter :as ff]]))

(def forge "test/resources/complexest/Input/FORGE_SE-99.txt")
(def forge-expected [["phase1" 10 42]
                     ["phase2" 43 98]])

(def forge-xl "test/resources/SupplyDemand.xlsx")
(def forge-xl-expected [["phase1" 1 33]
                        ["phase2" 34 89]])

(deftest phases-from-forge
  (testing "Are we parsing the phases from the SRC_by_Day worksheet
  properly when the worksheet is saved as tab delimitted text?"
    (is (= (vec (ff/processed-phases-from forge :start-day 10))
           forge-expected)))
  (testing "Are we parsing the phases from the SRC_by_Day worksheet
  properly when the worksheet is saved as xlsx with a worksheet name
  SRC_By_Day?"
    (is (= (vec (ff/processed-phases-from forge-xl))
           forge-xl-expected))))
