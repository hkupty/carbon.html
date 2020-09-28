(ns carbon.html
  (:require [carbon.template :as templ]
            [clojure.edn :as edn])
  (:gen-class))

(defn produce-content [template-path content]
  (templ/apply-template content (edn/read-string (slurp template-path))))

(defn -main [& args]
  (let [[template content] args]
    (print (produce-content template (edn/read-string (slurp content))))))
