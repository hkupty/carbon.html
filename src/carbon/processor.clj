(ns carbon.processor
  (:require [carbon.tags :as tags]
            [carbon.debug :as debug]
            [carbon.syntax :as syntax]
            [hiccup.page :as p]))

(defn binds [i]
  (some? ((into #{} (keys (methods syntax/carbon-bind))) i)))

(defn conds [i]
  (some? ((into #{} (keys (methods syntax/carbon-cond))) i)))

(defn tags [i]
  (some? ((into #{} (keys (methods tags/carbon-tag))) i)))

(def exported-fns
  {'odd? odd?
   'even? even?
   'true? true?
   'false? false?
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
                       (->> (debug/label ::tag) (get-method tags/carbon-tag))
                       (with-meta {:carbon? true :fn element})))
    (let [-key (first element)]

      (cond
        (binds -key) (debug/with-debug ::bind element
                       (let [{:keys [data]}
                             (apply syntax/carbon-bind element)]
                         data))

        (conds -key) (debug/with-debug ::cond element
                       (let [{:keys [data]}
                             (apply syntax/carbon-cond
                                    (update element 1 (comp first process vector)))]
                         data))

        :else element))))

(defn postprocess [element]
  (cond
    (and (vector? element)
         (or (fn? (first element))
             (var? (first element)))) (tags/run-fn element)
    :else element))


(defn process [tree]
  (debug/with-debug ::tree tree
    (let [is-vec? (vector? tree)
        xf (comp (map (partial debug/label ::node :start :tree tree))
                 (map preprocess)
                 (map process)
                 (map postprocess)
                 linearize
                 (map (partial debug/label ::node-finish tree))
                 )]
    (cond->> tree
        is-vec? (preprocess)
        is-vec? (into [] linearize)
        is-vec? (transduce
                  xf
                  conj!
                  (transient []))
        is-vec? persistent!))))

(defn render-page
  ([content tags] (render-page content (first tags) (second tags)))
  ([content head body]
   (binding [tags/*ctx* content]
     (debug/open-debug-gate!)
     (p/html5 {:mode :html}
              (process [:head head])
              (process [:body body])))))
