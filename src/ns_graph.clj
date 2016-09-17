(ns ns-graph
  {:boot/export-tasks true}
  (:require [boot.core         :as boot :refer [deftask]]
            [boot.util         :as util]
            [ns-graph.core     :as core]
            [ns-graph.plugin   :as plugin])
  (:import clojure.lang.ExceptionInfo))

(deftask draw
  "Generate a namespace dependecy graph."
  [s source-paths   PATH #{str} "Directories where to look for sources."
   d dest-dir       PATH str    "Where to save the resulting file."
   f format         VAL  str    "Format of the resulting image."
   n name           VAL  str    "Name to show in the graph title."
   l filename       VAL  str    "Name of the image file (without the extension)."
   x exclude        MASK #{str} "Namespaces/classes or masks to exclude from the graph."
   i include        MASK #{str} "Namespaces/classes or masks to additionally include in the graph."
   t title          VAL  kw     "Type of the graph title."
   o only-own            bool   "Draw only namespaces and classes from this project."
   a abbrev-ns           bool   "Abbreviate all but one parts of the namespace/package."
   c cluster-lang        bool   "Draw namespaces and classes in two separate clusters."
   v view           BIN  str    "Don't save the image, but open it with the provided BIN."

   _ debug               bool   "Save the .dot file alongside the image file."
   _ no-color            bool   "Don't use separate colors for own and foreign elements."
   _ class-shape    VAL  str    "Shape for the Java files."
   _ ns-shape       VAL  str    "Shape for the Clojure files."
   _ default-color  VAL  str    "Color of the foreign objects."
   _ own-color      VAL  str    "Color of the own objects."]
  (boot/with-pre-wrap fileset
    (try (plugin/validate-depgraph-options *opts*)
         (catch ExceptionInfo e
           (util/exit-error (println (.getMessage e)))))
    (util/info "Drawing namespace graph...\n")
    (core/depgraph* *opts*)
    fileset))
