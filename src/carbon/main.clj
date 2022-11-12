(ns carbon.main
  (:require [carbon.processor :as processor]
            [carbon.syntax :as syntax]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(defn -main [& args]
(syntax/add-to-search-folders! "resources")
  (let [fname (first args)
        {:keys [template content]} (edn/read-string (slurp (str "samples/" fname ".edn")))
        target (str "build/" fname ".html")]
    (println "building" fname)
    (io/make-parents target)
    (spit target (processor/render-page content template))))
