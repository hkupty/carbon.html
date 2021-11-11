(ns carbon.util)

(defn map-vals
  ([-fn] (map (fn [[k v]] [k (-fn v)])))
  ([-fn coll] (into {} (map-vals -fn) coll)))

(defn map-keys
  ([-fn] (map (fn [[k v]] [(-fn k) v])))
  ([-fn coll] (into {} (map-keys -fn) coll)))

(defn with-options [args]
  (if (map? (first args))
           ((juxt first next) args)
           [{} args]))
