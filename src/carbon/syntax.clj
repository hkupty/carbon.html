(ns carbon.syntax
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [carbon.tags :as tags]
            [clojure.walk :refer [postwalk]]
            [carbon.util :as util]))

(defn tap [x] (println x) x)

(defn read-resource [resource]
  (edn/read-string (slurp (io/reader (io/resource resource)))))

(defn with-context [ctx data] (assoc data :ctx (merge tags/*ctx* ctx)))

;; Syntax: Either updates the context or requires pre-processing
;; so it can change underlying structure (i.e. like a macro)
(defmulti carbon-syntax (fn [tag & _] tag))
(defmethod carbon-syntax :default [_ ctx & args] (vec args))

(defmethod carbon-syntax :c/component -component
  ([tag component] {:data (cond-> component (string? component) (read-resource))})
  ([tag ctx component] (with-context ctx (-component tag component))))

(defmethod carbon-syntax :c/inspect [_ & exprs]
  (with-context {:inspect? true} {:data exprs}))

(defn replacer [sym-val]
  (fn -step [expr]
    (postwalk
      (fn -replace [i]
        (if (symbol? i)
          (get sym-val i i)
          i))
      expr)))

;; Bind: Binds one or more symbols to an expression.
;; Such expression will be evaluated.
;; Remaining expressions will have replaced the symbol by the value of the expression
(defmulti carbon-bind (fn [tag & _] tag))
(defmethod carbon-bind :default [_ ctx & args] (vec args))

(defmethod carbon-bind :c/let [_ binds & exprs]
  (let [sym-val (apply hash-map binds)]
    {:data (into [] (map (replacer sym-val)) exprs)}))

(defmethod carbon-bind :c/for [_ binds & exprs]
  (let [sym-map (apply hash-map binds)]
    {:data (persistent!
             (transduce
               (mapcat (fn [i] (map (replacer (util/map-vals #(nth % i) sym-map)) exprs)))
               conj!
               (transient [])
               (range (apply min (map count (vals sym-map))))))}))

(defmulti carbon-cond (fn [tag & _] tag))
(defmethod carbon-cond :default [_ ctx & args] (vec args))

(defmethod carbon-cond :c/if -if [_ condition true-branch false-branch]
  {:data (if (true? condition)
    true-branch
    false-branch)})


(defmethod carbon-cond :c/when -when [_ condition branch]
  {:data (when (true? (tap condition))
           branch)})

