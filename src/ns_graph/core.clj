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

#_(parse-clojure-file "/Users/alex/clojure/android/neko/src/clojure/neko/ui/menu.clj" any?)

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
            (let [[_ import] (re-find #"import\s+([\w_\.\*]+);" line)]
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

#_(parse-java-file "/Users/alex/projects/android/Abalone/src/com/bytopia/abalone/BoardRenderer.java" any?)

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

#_(parse-directories ["/Users/alex/clojure/cider-nrepl/"] any?)

(defn class-package [classname]
  (second (re-matches #"(.*)\..+" (name classname))))

(defn short-name [classname]
  (second (re-matches #".*\.(.+)" (name classname))))

(defn abbrev-name
  "Abbreviate a dot- and dash- separated string by first letter. Leave the last
  part intact unless `abbr-last` is true."
  [string & [abbr-last]]
  (let [parts (partition-by #{\. \-} string)]
    (str/join
     (if abbr-last
       (map first parts)
       (concat (map first (butlast parts)) (last parts))))))

#_(abbrev-name "clojure.common.utils")
#_(abbrev-name "clojure.dashed-name.foo.bar")
#_(abbrev-name "clojure.dashed-name.foo.baz-qux")

(defn compress-java-dependencies-to-packages
  "For case when `only-packages` is enabled, rewrite the parsed data so that only
  Java packages and links between them remain."
  [parsed-files]
  (let [;; Step 1: rewrite all Java class names to packages.
        parsed-files (mapv (fn [{:keys [name type] :as parsed-file}]
                             (if (= type :java)
                               (assoc parsed-file
                                      :name (symbol (class-package name))
                                      :type :java-package)
                               parsed-file))
                           parsed-files)
        ;; Step 2: find same Java packages and merge them.
        parsed-files (->> parsed-files
                          (group-by (juxt :type :name))
                          (mapv (fn [[[type name] files]]
                                  (if (= type :java-package)
                                    {:name name
                                     :type type
                                     :clojure-depends (vec (distinct (apply concat (map :clojure-depends files))))
                                     :java-depends (vec (distinct (apply concat (map :java-depends files))))}
                                    (first files)))))
        ;; Step 3: replace Java dependencies everywhere with package names.
        parsed-files (mapv (fn [parsed-file]
                             (update-in parsed-file [:java-depends]
                                        #(vec (distinct (map (comp symbol class-package) %)))))
                           parsed-files)]
    parsed-files))

#_(parse-directories ["/Users/alex/clojure/cider-nrepl/src/"] any?)

(defn parsed-files->graph-data [parsed-files only-packages]
  {:namespaces (set
                (concat (keep #(when (= (:type %) :clojure) (:name %)) parsed-files)
                        (mapcat :clojure-depends parsed-files)))
   :classes (if only-packages
              []
              (->> (concat (keep #(when (= (:type %) :java) (:name %))
                                 parsed-files)
                           (mapcat :java-depends parsed-files))
                   distinct
                   (group-by class-package)))
   :packages (if only-packages
               (set
                (->> (concat (keep #(when (= (:type %) :java-package) (:name %))
                                   parsed-files)
                             (mapcat :java-depends parsed-files))
                     distinct))
               [])
   :links (set
           (->> (for [{:keys [name type clojure-depends java-depends]} parsed-files]
                  (concat (for [dep clojure-depends]
                            [[name type] [dep :clojure]])
                          (for [dep java-depends]
                            [[name type] [dep :java]])))
                (apply concat)
                set))})

#_(time (parsed-files->graph-data (parse-directories ["/Users/alex/clojure/cider-nrepl/"] any?) true))

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
  [{:keys [namespaces classes packages links]} title include? own?
   {:keys [default-color own-color abbrev-ns cluster-lang ns-shape class-shape]}]
  (let [clojure-nodes (for [x namespaces :when (include? x)]
                        [(keyword (str "clj_" x))
                         {:label (if (and abbrev-ns (own? x))
                                   (abbrev-name (str x)) (str x))
                          :shape ns-shape
                          :color (if (own? x) own-color default-color)}])
        java-nodes (concat
                    (for [[package classes] classes
                          :let [own (own? (first classes))
                                package-label (if (and abbrev-ns own)
                                                (abbrev-name package true)
                                                package)
                                color (if own own-color default-color)
                                classes-nodes
                                (for [x classes :when (include? x)]
                                  [(keyword (str "java_" x))
                                   {:label (short-name x)
                                    :shape class-shape
                                    :color color}])]
                          :when (seq classes-nodes)]
                      (dot/subgraph (str "cluster_" (safe-name package))
                                    (cons {:label package-label,
                                           :color color} classes-nodes)))
                    (for [x packages :when (include? x)
                          :let [own (own? x)
                                package-label (str (if (and abbrev-ns own)
                                                     (abbrev-name (str x)) x)
                                                   ".*")
                                color (if own own-color default-color)]]
                      [(keyword (str "java_" x))
                       {:label package-label
                        :shape class-shape
                        :color color}]))
        edges (for [[[from-name from-type] [to-name to-type]] links
                    :when (and (include? from-name) (include? to-name))]
                [(keyword (str (if (= from-type :clojure) "clj_" "java_") from-name))
                 :>
                 (keyword (str (if (= to-type :clojure) "clj_" "java_") to-name))])]
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
   :view nil})

(def ^:private default-interesting-opts
  {:abbrev-ns false
   :no-color false
   :only-own false
   :only-packages false
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
  (let [{:keys [source-paths format filename debug view include exclude
                only-own only-packages] :as opts}
        (merge default-boring-opts default-interesting-opts opts)
        source-paths (if (coll? source-paths) source-paths [source-paths])
        imgfile (if view
                  (File/createTempFile filename (str "." format))
                  (io/file (str filename "." format)))
        include? (fn [ns]
                   (or (some (partial matches? ns) include)
                       (not-any? (partial matches? ns) exclude)))

        all-files (parse-directories source-paths include?)
        all-files (if only-packages
                    (compress-java-dependencies-to-packages all-files)
                    all-files)
        graph-data (parsed-files->graph-data all-files only-packages)
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
