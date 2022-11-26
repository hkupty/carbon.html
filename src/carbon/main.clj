(ns carbon.main
  (:require [carbon.processor :as processor]
            [carbon.syntax :as syntax]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(def sample-microdata
  {:schema/tech-article [:article {:itemscope true :itemtype "https://schema.org/TechArticle"}]
   :subtitle [:span {:itemprop "alternativeHeadline"}]
   :name [:span {:itemprop "name"}]
   :url [:a {:itemprop "url"}]
   :author [:span {:itemprop "author" :itemtype "https://schema.org/Person" :itemscope true}]
   :headline [:span {:itemprop "headline"}]
   })

(defn -main [& args]
(syntax/add-to-search-folders! "resources")
  (let [fname (first args)
        {:keys [template content]} (edn/read-string (slurp (str "samples/" fname ".edn")))
        target (str "build/" fname ".html")]
    (println "building" fname)
    (io/make-parents target)
    (binding [processor/microdata sample-microdata]
      (spit target (processor/render-page content template)))))
