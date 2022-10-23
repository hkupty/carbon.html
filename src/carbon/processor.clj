(ns carbon.processor
  (:require [carbon.tags :as tags]
            [carbon.debug :as debug]
            [carbon.syntax :as syntax]
            [hiccup.page :as p]))

(defn binds [i]
  (some? ((into #{} (keys (methods syntax/carbon-bind))) i)))

(defn conds [i]
  (some? ((into #{} (keys (methods syntax/carbon-cond))) i)))

(defn special-keys [i]
  (some? ((into #{} (keys (methods syntax/carbon-syntax))) i)))

(defn tags [i]
  (some? ((into #{} (keys (methods tags/carbon-tag))) i)))

(declare process)

(defn preprocess [element]
  (if (not (vector? element))
    (cond-> element
      (symbol? element) (-> (resolve) (or element))
      (tags element) (->
                       (->> (debug/label ::tag) (get-method tags/carbon-tag))
                       (with-meta {:carbon? true :fn element})))
    (let [-key (first element)]

      (cond
        (binds -key) (debug/with-debug ::bind -key

                       (let [{:keys [data ctx]}
                             (debug/label ::bind-processed
                                          (apply syntax/carbon-bind element))]
                         data))

        (conds -key) (debug/with-debug ::cond -key
                       (let [{:keys [data ctx]}
                             (debug/label
                               ::cond-processed
                               (apply syntax/carbon-cond (update element 1 (comp first process vector))))]
                         data))

        (special-keys -key) (debug/with-debug ::syntax -key
                              (let [{:keys [data ctx]}
                                    (debug/label ::syntax-processed
                                                 (apply syntax/carbon-syntax element))]
                                data))
        :else element))))

(defn postprocess [element]
  (cond
    (and (vector? element)
         (or (fn? (first element))
             (var? (first element)))) (do
                                        (debug/label ::run element)
                                        (debug/label ::return (tags/run-fn element)))
    :else (debug/label ::postprocess-nop element)))

(def linearize
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result item]
       (cond
         (and (vector? item)
             (seq item)
             (every? vector? item))
         (reduce xf result item)
         (some? item) (xf result item)
         :else result)))))

(defn process [tree]
  (let [is-vec? (vector? tree)
        is-map? (map? tree)
        xf (comp (map preprocess)
                 (map process)
                 (map postprocess)
                 linearize
                 (map (fn [x]
                        (debug/label ::end-of-process tree x))))]
    (debug/label ::process tree {:is-vec? is-vec? :is-map? is-map?})
    (cond->> tree
        is-map? (into {} xf)
        is-vec? (preprocess)
        is-vec? (transduce
                  xf
                  conj!
                  (transient []))
        is-vec? persistent!)))


(defn render-page
  ([content tags] (render-page content (first tags) (second tags)))
  ([content head body]
   (binding [tags/*ctx* content]
     (debug/open-debug-gate!)
     (p/html5 {:mode :html}
              (process [:head head])
              (process [:body body])))))
