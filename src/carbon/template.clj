(ns carbon.template
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk postwalk]]
            [hiccup.page :as p]
            [hiccup2.core :as h]
            [hiccup.util :as hu]
            [clojure.java.io :as io]))

;; TODO Split this namespace

(defn tap [x]
  (println x)
  x)

(defn with-options [args]
  (if (map? (first args))
           ((juxt first next) args)
           [{} args]))

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
  (let [[-fn & args] coll
        data (meta -fn)]
    (if (:carbon? data)
      (apply -fn (:fn data) ctx args)
      (apply -fn args))))

(declare process)

(defn zoom
  ([-map -path] (zoom -map -path nil))
  ([-map -path -default]
  ((if (coll? -path) get-in get) -map -path -default)))

;; Tags: Default postwalk evaluated tags
(defmulti carbon-tag (fn [tag & args] tag))
(defmethod carbon-tag :default -default [tag ctx & args] (into [tag]
                                                               args))
(defmethod carbon-tag :c/style -style [_ ctx & css]
  [:style
   (str/join "\n" css)])

(defmethod carbon-tag :c/css -css [_ ctx selector & attributes]
  (str selector " { "
       (str/join "; " (map (fn [[-k -v]]
                            (str (name -k) ": " -v)) (partition 2 attributes)))
       " }"))


(defmethod carbon-tag :c/get -get [_ ctx var-path & opt]
  [:path var-path :ctx ctx :opt opt]
  (apply zoom ctx var-path opt))
(defmethod carbon-tag :c/zoom -get [_ _ctx -map -path & opt] (apply zoom -map -path opt))
(defmethod carbon-tag :c/ctx -ctx [_ ctx] ctx)
(defmethod carbon-tag :c/map -map [_ ctx -fn coll]
  (into []
        (map (fn [elem] (run-fn [-fn elem] ctx)))
        coll))

(defmethod carbon-tag :c/include [_ path] (hu/raw-string (slurp path)))
(defmethod carbon-tag :c/str [_ _ & args] (pr-str args))

;; Syntax: Prewalk evaluated tags
(defmulti carbon-syntax (fn [tag & _] tag))
(defmethod carbon-syntax :default [ctx & args] (vec args))

(defmethod carbon-syntax :c/component -component
  ([tag ctx component]
   (-component tag ctx component {}))
 ([tag ctx component extra-ctx]
   (process (cond-> component
              (string? component) (read-resource))
            (merge ctx extra-ctx))))

(defmethod carbon-syntax :c/wrap -wrap
  ([tag ctx inner-tag coll]
   (-wrap tag ctx inner-tag {} coll))
  ([_ ctx tag opts coll]
   (process (into [tag opts]
                  coll)
            ctx)))

(defmethod carbon-syntax :c/partial [_ ctx -fn & args]
  (let [half-fn (apply partial (-fn (methods carbon-tag)) (process args ctx))]
    (fn [_ctx & new-args] (apply half-fn new-args))))

(defmethod carbon-syntax :c/let [_ ctx binds & exprs]
  (let [sym-val (into {}
                      (map (fn [[sym -val]]
                             [sym (process -val ctx)]))
                      (partition 2 binds))]
    (into [:div]
          (map (fn -step [expr]
                    (postwalk
                      (fn -replace [i]
                        (if (coll? i)
                          i
                          (get sym-val i i)))
                      expr)))
          exprs)))

(defmethod carbon-syntax :c/let* [_ ctx binds & exprs]
  (let [sym-val (into {}
                      (map (fn [[sym -val]]
                             [sym (process -val ctx)]))
                      (partition 2 binds))]
    (into []
          (mapcat (fn -step [expr]
                    (postwalk
                      (fn -replace [i]
                        (if (coll? i)
                          i
                          (get sym-val i i)))
                      expr)))
          exprs)))

(defn- for-impl [let-impl ctx bind & expr]
  (let [[key- coll] bind
        coll (process coll ctx)]
    (into []
          (map (fn [elem]
                    (process (into [let-impl [key- elem]]
                                   expr)
                             ctx)))
          coll)))

(defmethod carbon-syntax :c/for [_ ctx bind & expr]
  (apply for-impl :c/let ctx bind expr))

(defmethod carbon-syntax :c/for* [_ ctx bind & expr]
  (apply for-impl :c/let* ctx bind expr))

(defmethod carbon-syntax :c/if [_ ctx pred succ fail]
            (if (process pred ctx)
              succ
              fail))

(defn process-argument [ctx arg]
  (let [tags (into #{} (keys (methods carbon-tag)))]
    (cond
      (tags arg) (with-meta (get-method carbon-tag arg)
                            {:carbon? true
                             :fn arg})
      (and (vector? arg)
           (or (fn? (first arg))
               (var? (first arg)))) (run-fn arg ctx)
      (and (vector? arg) ;; solve nested [:x [:y] [[:z] [..]]] -> [:x [:y] [:z] [..]]
           (keyword? (first arg))
           (every? vector? (rest arg))) (into [(first arg)]
                                              (linearize)
                                              (rest arg))

      :else arg)))

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
