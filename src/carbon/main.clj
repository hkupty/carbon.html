(ns carbon.main
  (:require [carbon.processor :as processor]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(defn -main [& args]
  (let [fname (first args)
        {:keys [template content]} (edn/read-string (slurp (str "samples/" fname ".edn")))
        target (str "build/" fname ".html")]
    (println "building" fname)
    (io/make-parents target)
    (spit target (processor/render-page content (:head template) (:body template)))))
