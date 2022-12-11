(ns carbon.util)

(defn map-vals
  ([-fn] (map (fn [^clojure.lang.MapEntry [k v]] [k (-fn v)])))
  ([-fn coll] (into {} (map-vals -fn) coll)))

(defn map-keys
  ([-fn] (map (fn [^clojure.lang.MapEntry [k v]] [(-fn k) v])))
  ([-fn coll] (into {} (map-keys -fn) coll)))

(defn filter-vals
  ([-fn] (filter (comp -fn val)))
  ([-fn coll] (into {} (filter-vals -fn) coll)))

(defn filter-keys
  ([-fn] (filter (comp -fn key)))
  ([-fn coll] (into {} (filter-keys -fn) coll)))
