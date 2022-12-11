(ns carbon.tags
  (:require [carbon.util :refer [map-vals]]
            [carbon.opts :as opts]
            [carbon.params :as params]
            [carbon.symbols :as syms]
            [clojure.string :as str]))

(defn is-hiccup? [x] (and (vector? x)
                          (some-> x (first) (keyword?))))

(declare process-tree)

(defn mount [tree argmap components]
   (let [component-fn (get components (first tree))]
     (if (nil? component-fn)
       tree
       (try
         (-> tree
             (rest)
             (component-fn argmap components))
         (catch Exception ex
           (throw (ex-info (str "Unable to mount component "
                                (name (first tree))
                                " due to an exception")
                           {:component (first tree)
                            :args (rest tree)}
                           ex)))))))

(def xf-unwrap
  (fn xform [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result item]
       (cond
         (seq? item) (reduce xf result item)
         (nil? item) result
         :else (xf result item))))))

(defn process-hiccup [tree argmap components]
  (let [processed (mount tree argmap components)]
    (cond->> processed
      (is-hiccup? processed) (into [] (comp (map #(process-tree % argmap components))
                                            xf-unwrap)))))



(defn process-tree [tree argmap components]
  (cond-> tree
      (symbol? tree) (syms/process-symbol argmap)
      (map? tree) (->>
                    (into {}
                          (map-vals
                            (fn [-tree]
                              (cond-> -tree ;; No need to go nested in the maps for now
                                (symbol? -tree) (syms/process-symbol argmap)
                                (is-hiccup? -tree) (process-tree argmap components))))))
      (is-hiccup? tree) (-> (process-hiccup argmap components))))

(defn component [param-keys tree]
  (fn [component-args base-map components]
      (let [[opts param-values] (opts/get-opts component-args)]
        (process-tree
          (opts/with-opts tree opts)
          (into {}
                     (comp
                       params/xf-keyword-map
                       (map-vals #(syms/process-symbol % base-map)))
                (map vector param-keys param-values))
          components))))
