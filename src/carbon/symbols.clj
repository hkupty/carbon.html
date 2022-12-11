(ns carbon.symbols
  (:require [clojure.string :as str]))

(def ^:private find-dot (re-pattern #"\."))

(defn- search-symbol [sym params]
 (let [sym-name (name sym)
       sym-ns (namespace sym)
       resolved (params (keyword sym-ns sym-name))
       [prefix & rst] (some-> sym-ns (str/split find-dot 2))
       first-ns (some-> prefix (keyword) params)]
      (cond
        (some? resolved) resolved
        (some? first-ns) (search-symbol (keyword (some->> rst seq (str/join ".")) sym-name) first-ns)
        (some-> sym (meta) :nullable?) nil
        :else (throw (ex-info (str "Symbol '" (name sym) "' does not resolve to a known value") {:symbol sym :params params})))))

(defn process-symbol [sym params]
  (cond-> sym
    (symbol? sym) (search-symbol params)))
