(defproject demand_builder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [spork "0.2.0.2-SNAPSHOT"]
                 [incanter "1.5.6"]]
                   
  :main demand_builder.core
  ;:skip-aot demand_builder.core
  :aot [demand_builder.core]
  ;:target-path "target/%s"
  ;:profiles {:uberjar {:aot :all}}
  
  )
