(defproject demand_builder "0.1.5-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[seesaw "1.5.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/clojure "1.11.1"]
                 [spork "0.2.1.7-SNAPSHOT"]
                 [proc "0.3.4-SNAPSHOT"]
                 [joinr/incanter-core "1.9.3-SNAPSHOT"]
                 [joinr/incanter-charts "1.9.3-SNAPSHOT"]]
  :source-paths ["src" #_"../spork/src"]
  :resource-paths ["test/resources"]
  :main demand-builder.main
  :aot [demand-builder.main]
  ;;  :repl-options {:timeout 300000}
  :plugins [[reifyhealth/lein-git-down "0.4.1"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {proc  {:coordinates  fsdonks/proc}})
