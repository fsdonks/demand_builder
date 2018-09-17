;;Shim class for running marathon without
;;aot compilation issues.
;;entrypoint for demand builder gui.
(ns demand-builder.main
  (:gen-class :main true))

;;This is the main entry point for demand builder.
;;It's a good example of a shim-class, and
;;requires some arcane features to get things
;;working, since we're creating repls on other
;;threads.
(defn -main [& args]
  ;;clojure.set isn't imported by default, causing errors when
  ;;aot-compiling in some places.
  (require 'clojure.set)
  ;;if we don't use this, i.e. establish a root binding
  ;;for the *ns* var, we can't use in-ns later....
  ;;which leads to compile-time and run-time errors..
  (require  'clojure.java.io)

  (binding [*ns* *ns*]
    ;;rather than :require it in the ns-decl, we load it
    ;;at runtime.
    (require 'demand_builder.core)
    (in-ns 'demand_builder.core)
    ;;if we don't use resolve, then we get compile-time aot
    ;;dependency on marathon.core.  This allows us to shim the
    ;;class.
    ((resolve 'demand_builder.core/gui) :exit? true)))
