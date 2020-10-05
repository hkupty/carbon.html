(ns carbon.template
  (:require [clojure.edn :as edn]
            [clojure.walk :refer [prewalk postwalk]]
            [hiccup.page :as p]
            [hiccup2.core :as h]
            [hiccup.util :as hu]
            [clojure.java.io :as io]))

(defn linearize []
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result item]
       (if (every? vector? item)
         (reduce xf result item)
         (xf result item))))))

(defn read-resource [resource]
  (edn/read-string (slurp (io/reader (io/resource resource)))))

(defn namespaced? [kw]
  (and (keyword? kw)
         (some? (namespace kw))))

(defn run-fn [coll ctx]
  (let [[-fn & args] coll]
    (if (-> -fn meta :carbon?)
      (apply -fn ctx args)
      (apply -fn args))))

(declare process)

(defn zoom
  ([-map -path] (zoom -map -path nil))
  ([-map -path -default]
  ((if (coll? -path) get-in get) -map -path -default)))

;; Tags: Default postwalk evaluated tags
(defmulti carbon-tag (fn [tag & _] tag))
(defmethod carbon-tag :default [ctx & args] (vec args))
(defmethod carbon-tag :c/get [_ ctx var-path & opt] (apply zoom ctx var-path opt))
(defmethod carbon-tag :c/read-resource [_ _ resource] (read-resource resource))
(defmethod carbon-tag :c/component [_ ctx component & opt]
  (process (cond-> component
             (string? component) (read-resource))
           (or (first opt) ctx)))
(defmethod carbon-tag :c/ctx [_ ctx] ctx)
(defmethod carbon-tag :c/map [_ ctx -fn coll]
  (into []
        (map (fn [elem] (run-fn [-fn elem] ctx)))
        coll))

(defmethod carbon-tag :c/include [_ path] (hu/raw-string (slurp path)))

;; Syntax: Prewalk evaluated tags
(defmulti carbon-syntax (fn [tag & _] tag))
(defmethod carbon-syntax :default [ctx & args] (vec args))
(defmethod carbon-syntax :c/let [_ ctx binds & exprs]
  (let [sym-val (into {}
                      (map (fn [[sym -val]]
                             [sym (process -val ctx)]))
                      binds)]
    (into []
          (map (fn -step [expr]
                 (postwalk
                   (fn -replace [i]
                     (get sym-val i i)) expr)))
          exprs)))
(defmethod carbon-syntax :c/partial [_ ctx -fn & args]
  (let [half-fn (apply partial (-fn (methods carbon-tag)) (process args ctx))]
    (fn [_ctx & new-args] (apply half-fn new-args))))
(defmethod carbon-syntax :c/for [_ ctx bind & expr]
  (let [[key- coll] (update bind 1 #(process % ctx))]
    (into []
          (mapcat (fn [elem]
                    (apply carbon-syntax :c/let ctx [key- elem] expr)))
          coll)))

(defmethod carbon-syntax :c/if [_ ctx pred succ fail]
            (if (process pred ctx)
              succ
              fail))

(defn process-argument [ctx arg]
  (cond
    (namespaced? arg) (with-meta (get-method carbon-tag arg)
                                 {:carbon? true})
    (and (vector? arg)
         (or (fn? (first arg))
             (var? (first arg)))) (run-fn arg ctx)
    (and (vector? arg) ;; solve nested [:x [:y] [[:z] [..]]] -> [:x [:y] [:z] [..]]
         (keyword? (first arg))
         (every? vector? (rest arg))) (into [(first arg)]
                                            (linearize)
                                            (rest arg))
    :else arg))

(defn preprocess
  "Evaluate forms that require pre-computing condition and symbols that resolve to clojure functions"
  [tree ctx]
  (let [special-forms (into #{}
                            (comp
                              (map key)
                              (filter namespaced?))
                            (methods carbon-syntax))]
    (prewalk
      (fn [el]
        (cond
          (and (coll? el) (special-forms (first el))) (apply carbon-syntax (first el) ctx (rest el))
          (symbol? el) (or (resolve el) el)
          :else el))
      tree)))

(defn process [tree ctx]
  ;; We need to traverse twice, so some special forms can evaluate
  ;; their predicates without evaluating their arguments.
  ;; All other forms will evaluate their arguments before.
  (postwalk (partial process-argument ctx)
    (preprocess tree ctx)))

(defn render [tag templ content]
  (into [tag]
          (process templ content)))

(defn apply-template [content {:keys [head body]}]
  (p/html5 {:mode :html}
           (render :head head content)
           (render :body body content)))
