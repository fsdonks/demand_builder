(defproject demand_builder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [spork "0.2.1.0-SNAPSHOT"]
                 [proc "0.2.6-SNAPSHOT"]
                 ;;should be updated to 1.9.3
                 [incanter "1.9.3"]]
  :source-paths ["src" "../spork/src"]
  :main demand_builder.core
  ;:skip-aot demand_builder.core
  :aot [demand_builder.core
        ;;this doesn't need to be aot'd
        demand_builder.chart])
  ;:target-path "target/%s"
  ;:profiles {:uberjar {:aot :all}}
  ;:profiles {:dev {:resource-paths ["test/resources"]}}
  
