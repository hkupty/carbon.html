(ns carbon.processor
  (:require [carbon.tags :as tags]
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

(def ^:dynamic replaced-ctx? nil)

(def debug? (atom false))

(defn tap [x] (println x) x)
(defn label [& args] (when @debug?
                       (apply println args))
  (last args))

(declare process)

(defn preprocess [element]
  (cond
    (not (vector? element)) (cond-> element
                              (symbol? element) (->
                                                  (resolve)
                                                  (or element))
                              (tags element) (->
                                               (->> (label ::tag)
                                                    (get-method tags/carbon-tag))
                                               (with-meta {:carbon? true :fn element})))

    (binds (first element)) (do
                              (label ::bind (first element))
                              (reset! debug? true)
                              (let [{:keys [data ctx]}
                                    (apply syntax/carbon-bind (update element 1 process))]
                              (reset! debug? false)

                                (when (some? ctx)
                                  (push-thread-bindings {#'tags/*ctx* ctx})
                                  (set! replaced-ctx? true))

                                data))

    (conds (first element)) (do
                              (label ::cond (first element))
                              (reset! debug? true)
                              (let [{:keys [data ctx]}
                                    (apply syntax/carbon-cond (update element 1 (comp process vector)))]
                              (reset! debug? false)

                                (when (some? ctx)
                                  (push-thread-bindings {#'tags/*ctx* ctx})
                                  (set! replaced-ctx? true))

                                data))

    (special-keys (first element)) (let [{:keys [data ctx]}
                                         (apply syntax/carbon-syntax element)]

                                     (when (some? ctx)
                                       (push-thread-bindings {#'tags/*ctx* ctx})
                                       (set! replaced-ctx? true))

                                     data)
    :else element))

(defn postprocess [element]
  (cond
    (and (vector? element)
         (or (fn? (first element))
             (var? (first element)))) (do
                                        (label ::run element)
                                        (label ::return (tags/run-fn element)))
    :else (label ::postprocess-nop element)))

(defn cleanup [element]
  (when replaced-ctx?
    (when (-> tags/*ctx* :inspect?)
      (pr-str element))
    (set! replaced-ctx? nil)
    (pop-thread-bindings))
  element)

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
  (label ::process tree)
  (let [is-vec? (vector? tree)
        is-map? (map? tree)
        xf (comp (map preprocess)
                 (map process)
                 (map postprocess)
                 (map cleanup)
                 linearize
                 (map (fn [x]
                        (label ::end-of-process tree x))))]
    (binding [replaced-ctx? nil] ;; Needed for set! to work
      (cond->> tree
        is-map? (into {} xf)
        is-vec? (transduce
                  xf
                  conj!
                  (transient []))
        is-vec? persistent!))))


(defn render-page
  ([content tags] (render-page content (first tags) (second tags)))
  ([content head body]
   (binding [tags/*ctx* content]
     (p/html5 {:mode :html}
              (process [:head head])
              (process [:body body])))))

(comment

  (binding [tags/*ctx* {:parts '({:path "http://xx.yy" :name "xx.yy"}
                                 {:path "http://yy.zz" :name "yy.zz"})}]
    (process [[:c/for ['part [:c/get [:parts]]]
     [:li [:a {:href [:c/zoom 'part [:path]]} [:c/zoom 'part [:name]]]]]]))


  (binding [tags/*ctx* {:names {:long "Stuff" :short "S"}}]
    (process [[:c/let ['x [:c/get [:names]]] [:span 'x]]]))







  )
