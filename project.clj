(defproject demand_builder "0.1.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[seesaw "1.5.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/clojure "1.10.1"]
                 [spork "0.2.1.4-SNAPSHOT"]
                 [proc "0.2.8-SNAPSHOT"]
                 ;;should be updated to 1.9.3
                 [joinr/incanter-core "1.9.3-SNAPSHOT"]
                 [joinr/incanter-charts "1.9.3-SNAPSHOT"]]
  :source-paths ["src" #_"../spork/src"]
  :main demand-builder.main
  :aot [demand-builder.main]
;  :repl-options {:timeout 300000}
  )
