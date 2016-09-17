(ns ns-graph.plugin
  "Parts that are common to both Boot and Leiningen plugins.")

(defn- throw-ex [message]
  (throw (ex-info message {})))

(def allowed-title-types #{:name :name-and-opts :name-and-pprint-opts})
(def allowed-image-formats #{"png" "svg"})

(defn validate-depgraph-options
  "Throws exception if validation fails."
  [{:keys [source-paths title format] :as opts}]
  (when (empty? source-paths)
    (throw-ex "At least one source-path must be provided."))
  (when (and title (not (allowed-title-types title)))
    (throw-ex (str "Title type must be one of: " allowed-title-types)))
  (when (and format (not (allowed-image-formats format)))
    (throw-ex (str "Format must be one of: " allowed-image-formats))))
