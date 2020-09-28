(ns carbon.template
  (:require [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]
            [hiccup.page :as p]
            [hiccup2.core :as h]
            [hiccup.util :as hu]
            [clojure.java.io :as io]))

(defn read-resource [resource]
  (edn/read-string (slurp (io/reader (io/resource resource)))))

(def template-functions
  #:carbon.fn{:str str})

(defn namespaced-as [-ns kw]
  (and (keyword? kw)
         (= -ns (namespace kw))))

(defn run-fn [coll ctx]
  (let [[-fn & args] coll]
    (apply -fn ctx args)))

(declare process)
(declare process-argument)

(def render-template
  #:carbon.template{:default (fn -default [ctx var-path templ]
                               (let [arg (get-in ctx var-path)]
                                 (process-argument ctx
                                   (if (some? arg)
                                     arg
                                     templ))))
                    :get (fn -get [ctx var-path]
                              (get-in ctx var-path))
                    :extend-with (fn -extend-with [ctx var-name]
                                   (process (read-resource (get-in ctx var-name))
                                            ctx))
                    :extend-from (fn -extend-from [ctx path]
                                   (process (read-resource path)
                                            ctx))
                    :component (fn -component [ctx path ctx-path]
                                 (process (read-resource path)
                                          (get-in ctx ctx-path)))
                    :map (fn -map [ctx tag -fn & args]
                           (let [fn-args (vec (butlast args))
                                 coll (last args)
                                 render-fn (get render-template -fn)]
                             (into [tag]
                                 (map (fn [item]
                                        (apply render-fn ctx (conj fn-args item))))
                                 coll)))
                    :include (fn -include [_ path]
                               (hu/raw-string (slurp path)))})


(defn process-argument [ctx arg]
  (cond
    (namespaced-as "carbon.template" arg) (render-template arg)
    (namespaced-as "carbon.fn" arg) (template-functions arg)
    (and (vector? arg)
         (fn? (first arg))) (run-fn arg ctx)
    (vector? arg) (mapv (partial process-argument ctx) arg)
    :else arg))


(defn process [tree ctx]
  (postwalk (partial process-argument ctx)
    tree))

(defn render [tag templ content]
  [tag
   (process templ content)])

(defn apply-template [content {:keys [head body]}]
  (p/html5 {}
           (render :head head content)
           (render :body body content)))
