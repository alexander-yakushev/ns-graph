(set-env! :source-paths #{"src"}
          :resource-paths #{"src"}
          :dependencies '[[org.clojure/clojure "1.9.0" :scope "provided"]
                          [dorothy "0.0.5" :exclusions [org.clojure/clojure]]])

(task-options!
 pom {:project     'ns-graph
      :version     "0.1.3"
      :description "Library for charting dependencies between namespaces and classes."
      :url         "https://github.com/alexander-yakushev/ns-graph"
      :scm         {:url "https://github.com/alexander-yakushev/ns-graph"}
      :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
 push {:repo "clojars"})

(deftask build []
  (comp (pom) (jar)))
