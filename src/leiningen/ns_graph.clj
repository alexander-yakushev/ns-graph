(ns leiningen.ns-graph
  "Leiningen plugin."
  (:require [ns-graph.core :as core]
            [ns-graph.plugin :as plugin]
            [leiningen.core.main :as main])
  (:import clojure.lang.ExceptionInfo))

(defn ns-graph [project & _]
  (let [opts (merge {:name (str (:name project))
                     :source-paths (concat (:source-paths project)
                                           (:java-source-paths project))}
                    (:ns-graph project))]
    (try (plugin/validate-depgraph-options opts)
         (catch ExceptionInfo e
           (main/abort (.getMessage e))))
    (main/info "Drawing namespace graph...")
    (core/depgraph* opts)))
