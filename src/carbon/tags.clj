(ns carbon.tags
  (:require [clojure.string :as str]))

(def ^:dynamic *ctx* {})

(defn run-fn [coll]
  (let [[-fn & args] coll
        data (meta -fn)]
    (if (:carbon? data)
      (apply -fn (:fn data) args)
      (apply -fn args))))

(defn path? [-val] (or (coll? -val)
                       (keyword? -val)))

(defn zoom
  ([-path] (zoom *ctx* -path nil))
  ([-map -path] (zoom -map -path nil))
  ([-map -path -default]
  ((if (coll? -path) get-in get) -map -path -default)))

(defn slug [-str]
  (-> -str
      (str/trim)
      (str/replace #" " "_")
      (str/lower-case)))

(defmulti carbon-tag (fn [tag & args] tag))
(defmethod carbon-tag :default -default [tag & args] (into [tag] args))

(defmethod carbon-tag :c/style -style [_ & css] [:style (str/join "\n" css)])

(defmethod carbon-tag :c/css -css [_ selector & attributes]
  (str selector " { "
       (str/join "; " (map (fn [[-k -v]]
                            (str (name -k) ": " -v)) (partition 2 attributes)))
       " }"))


(defmethod carbon-tag :c/get -get [_ var-path & opt]
  (apply zoom *ctx* var-path opt))

(defmethod carbon-tag :c/zoom -zoom [_ -map -path & opt] (apply zoom -map -path opt))

#_(defmethod carbon-tag :c/link -link [_ -map var-path & opt]
  (apply zoom *ctx* (zoom -map var-path) opt))

#_(defmethod carbon-tag :c/ctx -ctx [_] *ctx*)
(defmethod carbon-tag :c/map -map [_ -fn coll]
  (into []
        (map (fn [elem] (run-fn [-fn elem])))
        coll))

(defmethod carbon-tag :c/merge -merge [_ & coll]
  (apply merge coll))

(defmethod carbon-tag :c/slug -slug [_ & val-or-path]
  (slug (if (not (string? (first val-or-path)))
          (apply zoom val-or-path)
      (first val-or-path))))

(defmethod carbon-tag :c/id -id [_ & val-or-path]
  (str "#" (slug (if (not (string? (first val-or-path)))
          (apply zoom val-or-path)
      (first val-or-path)))))

(defmethod carbon-tag :c/kv -kv [_ -key]
  {(cond-> -key
    (vector? -key) (last))
   (zoom *ctx* -key)})

#_(defmethod carbon-tag :c/str [_ _ & args] (pr-str args))

