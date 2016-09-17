(ns ns-graph.core
  "Generate a namespace dependency graph as an image."
  (:require [dorothy.core :as dot]
            [clojure.string :as s]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.string :as str])
  (:import java.io.File))

(defn read-ns [file]
  (with-open [f (-> file io/reader java.io.PushbackReader.)]
    (binding [*in* f]
      (let [x (read)]
        (when (= (first x) 'ns)
          x)))))

#_(read-ns "src/ns_graph.clj")
#_(read-ns "build.boot")

(defn parse-reference-entry [ref]
  (if-not (sequential? ref)
    [ref]
    (let [ext (loop [[c & r] (rest ref), ns-ext ()]
                (if (or (keyword? c) (nil? c))
                  ns-ext
                  (recur r (conj ns-ext (if (sequential? c)
                                          (first c) c)))))]
      (if (empty? ext)
        [(first ref)]
        (map #(symbol (str (first ref) \. %)) ext)))))

#_(parse-reference-entry 'clojure.pprint)
#_(parse-reference-entry '[clojure.pprint :as pp])
#_(parse-reference-entry '[clojure pprint zip])
#_(parse-reference-entry '[clojure.java [io :refer [file]] sh])

(defn parse-ns-subform
  "Parses references entries under the given `key` in `ns-form`."
  [ns-form key]
  (->> ns-form
       (filter #(and (coll? %) (= key (first %))))
       (mapcat rest)
       (mapcat parse-reference-entry)
       set))

(defn parse-clojure-file [f include?]
  (when-let [[_ ns :as ns-form] (read-ns f)]
    (when (include? ns)
      {:name ns
       :type :clojure
       :clojure-depends (concat (parse-ns-subform ns-form :require)
                                (parse-ns-subform ns-form :use))
       :java-depends (parse-ns-subform ns-form :import)})))

#_(parse-clojure-file "/home/unlogic/clojure/android/neko/src/clojure/neko/ui/menu.clj" [])

(defn java-get-classname [short-filename content-lines]
  (let [[_ package] (some #(re-find #"package\s+([\w\._]*)\s*;" %) content-lines)
        [_ class] (re-matches #"(.+)\.java" short-filename)]
    (symbol (str package "." class))))

#_(java-get-classname "BoardRenderer.java" (line-seq (io/reader "/home/unlogic/projects/android/Abalone/src/com/bytopia/abalone/BoardRenderer.java")))

(defn java-get-imports [content-lines]
  (loop [[^String line & r] content-lines
         imports []]
    (if line
      (cond (.startsWith (.trim line) "import")
            (let [[_ import] (re-find #"import\s+([\w_\.\*]+);" line)
                  import (if (.endsWith import ".*")
                           (second (re-matches #"(.+)\.\*" import))
                           import)]
              (recur r (conj imports (symbol import))))

            (re-matches #"\s*class\s+[\w_]+" line)
            imports

            :else (recur r imports))
      imports)))

#_(java-get-imports (line-seq (io/reader "/home/unlogic/projects/android/Abalone/src/com/bytopia/abalone/BoardRenderer.java")))

(defn parse-java-file [f include?]
  (let [contents (line-seq (io/reader f))
        cn (java-get-classname (.getName (io/file f)) contents)]
    (when (include? cn)
      {:name cn
       :type :java
       :clojure-depends []
       :java-depends (java-get-imports contents)})))

#_(parse-java-file "/home/unlogic/projects/android/Abalone/src/com/bytopia/abalone/BoardRenderer.java" [])

(defn all-clojure-files [root]
  (->> (file-seq (io/file root))
       (filter #(.endsWith (str %) ".clj"))))

#_(all-clojure-files "/home/unlogic/clojure/ns-graph/")

(defn all-java-files [root]
  (->> (file-seq (io/file root))
       (filter #(.endsWith (str %) ".java"))))

#_(all-java-files  "/home/unlogic/projects/android/Abalone/")

(defn parse-directories
  "Find all Java and Clojure sources in the given `dirs` and returns a list of
  parsed structure for each file. Include only namespaces and classes which pass
  `include?` predicate."
  [dirs include?]
  (mapcat (fn [dir]
            (concat (keep #(parse-clojure-file % include?) (all-clojure-files dir))
                    (keep #(parse-java-file % include?) (all-java-files dir))))
          dirs))

#_(parse-directories ["/home/unlogic/clojure/cider-nrepl/"] [])

(defn class-package [classname]
  (second (re-matches #"(.*)\..+" (name classname))))

(defn short-name [classname]
  (second (re-matches #".*\.(.+)" (name classname))))

(defn abbrev-name [string]
  (let [parts (partition-by #{\. \-} string)]
    (-> (map first (butlast parts))
        (concat (last parts))
        str/join)))

#_(abbrev-name "clojure.common.utils")
#_(abbrev-name "clojure.dashed-name.foo.bar")
#_(abbrev-name "clojure.dashed-name.foo.baz-qux")

(defn parsed-files->graph-data [parsed-files]
  {:namespaces (set
                (concat (keep #(when (= (:type %) :clojure) (:name %)) parsed-files)
                        (mapcat :clojure-depends parsed-files)))
   :classes (->> (concat (keep #(when (= (:type %) :java) (:name %))
                               parsed-files)
                         (mapcat :java-depends parsed-files))
                 distinct
                 (group-by class-package))
   :links (set
           (for [file parsed-files
                 dep (concat (:java-depends file) (:clojure-depends file))]
             [(:name file) dep]))})

#_(time (parsed-files->graph-data (parse-directories ["/home/unlogic/clojure/cider-nrepl/"] (constantly true))))

#_(generate-graph (parsed-files->graph-data (parse-directory "/home/unlogic/clojure/cider-nrepl/" [])) false)

(defn safe-name [string]
  (str (.replaceAll (str string) "(\\.|-|\\$)" "_")))

(defn- matches? [ns-or-class template]
  (let [lng (dec (count template))]
    (if (= (.charAt template lng) \*)
      (.startsWith (str ns-or-class) (subs template 0 lng))
      (= template (str ns-or-class)))))

(defn generate-graph
  "Given graph data, generates almost final DOT-file representation."
  [{:keys [namespaces classes links]} title include? own?
   {:keys [default-color own-color abbrev-ns cluster-lang ns-shape class-shape]}]
  (let [clojure-nodes (for [x namespaces :when (include? x)]
                        [(keyword x) {:label (if abbrev-ns
                                               (abbrev-name (str x)) (str x))
                                      :shape ns-shape
                                      :color (if (own? x) own-color default-color)}])
        java-nodes (for [[package classes] classes
                         :let [color (if (own? (first classes)) own-color default-color)
                               classes-nodes
                               (for [x classes :when (include? x)]
                                 [(keyword x) {:label (short-name x)
                                               :shape class-shape
                                               :color color}])]
                         :when (seq classes-nodes)]
                     (dot/subgraph (str "cluster_" (safe-name package))
                                   (cons {:label package, :color color} classes-nodes)))
        edges (for [[from to] links
                    :when (and (include? from) (include? to))]
                [(keyword from) :> (keyword to)])]
    (dot/digraph :simple_hierarchy
                 (concat [{:rankdir :LR, :label title}]
                         edges
                         (if cluster-lang
                           [(dot/subgraph :cluster_clojure
                                          (cons {:label "clojure"}
                                                clojure-nodes))]
                           clojure-nodes)
                         (if cluster-lang
                           [(dot/subgraph :cluster_java
                                          (cons {:label "java"}
                                                java-nodes))]
                           java-nodes)))))

(def ^:private default-boring-opts
  {:default-color "black"
   :own-color "blue"
   :ns-shape "ellipse"
   :class-shape "hexagon"
   :title :name-and-opts
   :name "Default graph"
   :format "png"
   :filename "graph"
   :dest-dir "./"
   :view nil})

(def ^:private default-interesting-opts
  {:abbrev-ns false
   :no-color false
   :only-own false
   :cluster-lang false})

(defn graph-title
  "Returns a string for the graph caption that contains the name, and possibly
  the configuration."
  [{:keys [title name] :as opts}]
  (let [differing-opts (first (diff opts default-interesting-opts))]
    (if (= title :name)
      name
      (format "%s %s"
              name
              (with-out-str
                ((if (= title :name-and-pprint-opts)
                   pprint println)
                 (apply dissoc differing-opts :source-paths
                        (keys default-boring-opts))))))))

(defn depgraph*
  "Function that generates a namespace dependency graph given the map of options."
  [opts]
  (let [{:keys [source-paths dest-dir format filename debug view include exclude only-own]
         :or {dest-dir "./", format "png", filename "graph"}
         :as opts} (merge default-boring-opts default-interesting-opts opts)
        source-paths (if (coll? source-paths) source-paths [source-paths])
        imgfile (if view
                  (File/createTempFile filename (str "." format))
                  (io/file (str filename "." format)))
        include? (fn [ns]
                   (or (some (partial matches? ns) include)
                       (not-any? (partial matches? ns) exclude)))

        all-files (parse-directories source-paths include?)
        graph-data (parsed-files->graph-data all-files)
        ;; Set of all processed files is a predicate to check if a file is from
        ;; own project.
        own? (set (map :name all-files))
        ;; Enhance include? with checking if the namespace/class belongs to the
        ;; project.
        include? (if only-own
                   (fn [ns]
                     (or (some (partial matches? ns) include)
                         (and (own? ns)
                              (not-any? (partial matches? ns) exclude))))
                   include?)

        title (graph-title opts)
        graph (-> graph-data
                  (generate-graph title include? own? opts)
                  dot/dot)]
    (when debug
      (spit (io/file (str filename ".dot")) graph))
    (dot/save! graph imgfile {:format (keyword format)})

    (when view
      (sh view (str imgfile)))))
