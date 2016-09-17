(set-env! :source-paths #{"src"}
          :dependencies '[[adzerk/bootlaces "0.1.13" :scope "test"]
                          [dorothy "0.0.5"]])

(require '[adzerk.bootlaces :refer :all])

(def version "0.1.0")

(bootlaces! version)

(task-options!
 pom {:project     'ns-graph
      :version     version
      :description "Library for charting dependencies between namespaces and classes."
      :url         "https://github.com/alexander-yakushev/ns-graph"
      :scm         {:url "https://github.com/alexander-yakushev/ns-graph"}
      :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
 push {:repo "clojars"})

(deftask deploy "" []
  (comp (pom) (jar) (target) (push)))
