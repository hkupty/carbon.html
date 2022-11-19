(ns carbon.processor
  (:require [carbon.tags :as tags]
            [carbon.debug :as debug]
            [carbon.syntax :as syntax]
            [hiccup.page :refer [doctype]]
            [hiccup.core :refer [html]]))

(def binds (into #{} (keys (methods syntax/carbon-bind))))

(def conds (into #{} (keys (methods syntax/carbon-cond))))

(def tags (into #{} (keys (methods tags/carbon-tag))))

(def exported-fns
  {'odd? odd?
   'even? even?
   'true? true?
   'false? false?
   'some? some?
   '= =
   '> >
   '< <
   '>= >=
   '<= <=
   'str str
   '* *
   '+ +
   '- -
   '/ /})

(declare process)

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

(defn preprocess [element]
  (if (not (vector? element))
    (cond-> element
      (symbol? element) (-> (exported-fns) (or element))
      (tags element) (->
                       (->> (get-method tags/carbon-tag))
                       (with-meta {:carbon? true :fn element})))
    (let [[-key bind & forms] element]

      (cond
        (binds -key) (apply syntax/carbon-bind -key bind forms)

        (conds -key) (apply syntax/carbon-cond
                                    -key
                                    (first (process (vector bind)))
                                    forms)

        :else element))))

(defn postprocess [element]
  (cond
    (and (vector? element)
         (or (fn? (first element))
             (var? (first element)))) (tags/run-fn element)
    :else element))


(declare process-xf)

(defn process [tree]
  (let [is-vec? (vector? tree)
        is-map? (map? tree)]
    (cond->> tree
        is-vec? (preprocess) ;; preprocess tree
        (or is-vec?
            is-map?) (transduce
                  process-xf
                  conj
                  (or (empty tree) [])))))
(def process-xf
  (comp (map preprocess)
        (map process)
        (map postprocess)
        linearize))

(defn render [tree context]
  (binding [tags/*ctx* context]
    (process tree)))

(defn render-page [data content]
  (html {:mode :html}
        (doctype :html5)
        (render content data)))
