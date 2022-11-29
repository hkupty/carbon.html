(ns carbon.v2.tags
  (:require [clojure.walk :refer [postwalk prewalk]]
            [carbon.util :refer [map-vals]]
            [carbon.v2.opts :as opts]
            [carbon.v2.params :as params]
            [carbon.v2.symbols :as syms]
            [clojure.string :as str]))

(defn tap [x] (println x) x)
(defn label [& x] (println x) (last x))

;; TODO find a way to safely recurse without requring within-bind
(def ^:private ^:dynamic within-bind false)
(def ^:private ^:dynamic components {})

(def linearize
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result item]
       (cond
         (and (vector? item) (seq item) (every? vector? item)) (reduce xf result item)
         (some? item) (xf result item)
         :else result)))))

(declare process-tree)

(defn mount
  ([tree argmap -components]
   (binding [components -components]
     (mount tree argmap)))
  ([tree argmap]
   (let [-tree (if-let [component-fn (get components (first tree))]
                 (component-fn (rest tree) argmap)
                 tree)
         -tree (if (not within-bind)
                 (binding [within-bind true]
                   (process-tree -tree argmap))
                 -tree)]
     (cond->> -tree
      (sequential? -tree) (into [] linearize)))))

(defn process [argmap]
  (fn [tree]
    (cond
      (symbol? tree) (syms/process-symbol tree argmap)
      (and (vector? tree)
           (not (map-entry? tree))
           (keyword? (first tree))) (mount tree argmap)
      :else tree)))

(defn process-tree [tree argmap] (prewalk (process argmap) tree))

(defn component [param-keys body]
  (fn [component-args base-params]
      (let [[opts param-values] (opts/get-opts component-args)]
        (process-tree
          (opts/with-opts body opts)
          (params/with-new-items base-params param-keys param-values)))))

