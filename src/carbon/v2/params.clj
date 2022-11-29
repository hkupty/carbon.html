(ns carbon.v2.params
  (:require [carbon.v2.symbols :as syms]
            [carbon.util :refer [map-keys]]))

(def ^:private -xf-keyword-map (map-keys (comp keyword name)))

(defn merge-with-transform-nested [base xf target]
  (transduce
    xf
    (completing (fn [acc [k v]]
      (if (and (some? (get acc k))
             (map? v))
        (update acc k merge-with-transform-nested xf v)
        (assoc acc k v))))
    base
    target))

(defn merge-params [base-params new-params]
  (merge-with-transform-nested base-params -xf-keyword-map new-params))

(defn as-keyword-map [-map]
  (transduce
    -xf-keyword-map
    (completing (fn [acc [k v]]
      (assoc acc k (cond-> v (map? v) (as-keyword-map)))))
    {}
    -map))

(defn with-new-items [base-params param-keys param-values]
  (merge-params
    base-params
    (into {}
          (map vector
               param-keys
               (map (fn [sym] (syms/process-symbol sym base-params))
                    param-values)))))
