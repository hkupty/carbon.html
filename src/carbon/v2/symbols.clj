(ns carbon.v2.symbols
  (:require [clojure.string :as str]))

(defn process-symbol [sym params]
  (if (not (symbol? sym))
    sym
    (if-let [resolved (params (keyword (name sym)))]
      resolved
      (if-let [resolved (some-> sym
                            namespace
                            (str/split #"\.")
                            (conj (name sym))
                            (->>
                              (map keyword)
                              (get-in params)))]
      resolved
      sym))))
