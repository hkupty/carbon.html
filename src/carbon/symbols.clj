(ns carbon.symbols
  (:require [clojure.string :as str]))

(defn process-symbol [sym params]
  (if (not (symbol? sym))
    sym
    (let [resolved (params (keyword (namespace sym) (name sym)))]
      (if (some? resolved)
      resolved
      (if (some-> sym (meta) :nullable?)
        nil
        (throw (ex-info (str "Symbol '" (name sym) "' does not resolve to a known value") {:symbol sym :params params})))))))
