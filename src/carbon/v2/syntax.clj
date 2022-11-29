(ns carbon.v2.syntax
  (:require [carbon.v2.tags :as tags-v2]
            [carbon.v2.symbols :as syms]
            [carbon.v2.params :as params]
            [carbon.v2.opts :as opts]
            [carbon.util :refer [map-vals]]
            [clojure.string :as str]))

(defn produce-linear
  ([data] (produce-linear data {}))
  ([data linear]
   (let [[-k vs] (first data)
         iter (rest data)]
     (if (seq iter)
       (mapcat (fn [v] (produce-linear iter (assoc linear -k v))) vs)
       (map (fn [v] (assoc linear -k v)) vs)))))

(defn evaluate [expr]
  (cond
    (symbol? expr) (resolve expr)
    (vector? expr) (apply (or (resolve (first expr)) (constantly false))
                          (rest expr))
    :else expr))

(defn slug [-str]
  (-> -str
      (str/trim)
      (str/replace #" " "_")
      (str/lower-case)))

;;; Exported tags

(defn carbon-for [[binds & forms] base-map]
    (->> binds
         (partition 2)
         (map-vals (fn [sym] (syms/process-symbol sym base-map)))
         (produce-linear)
         (mapcat (fn [argmap] (tags-v2/process-tree forms (params/merge-params base-map argmap))))))

(defn carbon-if [[pred form-true form-false] argmap]
    (if (evaluate (tags-v2/process-tree pred argmap))
      (tags-v2/process-tree form-true argmap)
      (tags-v2/process-tree form-false argmap)))

(defn carbon-when [[pred form-true] argmap]
    (when (evaluate (tags-v2/process-tree pred argmap))
      (tags-v2/process-tree form-true argmap)))


(defn carbon-slug [[sym] base-map]
  (slug (cond-> sym
    (symbol? sym) (syms/process-symbol base-map))))

(defn carbon-id [[sym] base-map]
  (->> (cond-> sym (symbol? sym) (syms/process-symbol base-map))
     (slug)
     (str "#")))

(defn carbon-set-option [[component opts] base-map]
  (opts/with-opts (syms/process-symbol component base-map) opts))

(defn carbon-merge [[& maps] base-map]
  (params/merge-with-transform-nested
    {}
    (comp
      cat
      (map-vals #(tags-v2/process-tree % base-map)))
    maps))

(defn carbon-defaults [[default-map form] base-map]
  (tags-v2/process-tree form
                        (params/merge-params (params/as-keyword-map default-map) base-map)))

(def default-tags
  {:for carbon-for
   :if carbon-if
   :when carbon-when
   :merge carbon-merge
   :slug carbon-slug
   :id carbon-id
   :with-options carbon-set-option
   :with-defaults carbon-defaults})
