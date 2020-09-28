(ns carbon.template
  (:require [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]
            [hiccup.page :as p]
            [hiccup2.core :as h]
            [hiccup.util :as hu]
            [clojure.java.io :as io]))

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
  #:carbon.template{:default (fn -default [ctx var-name templ]
                               (let [arg (get ctx var-name)]
                                 (process-argument ctx
                                   (if (some? arg)
                                     arg
                                     templ))))
                    :get-in (fn -get-in [ctx var-path]
                              (get-in ctx var-path))
                    :get (fn -get [ctx var-name]
                           (get ctx var-name))
                    :extend-with (fn -extend-with [ctx var-name]
                                   (process (edn/read-string (slurp (io/reader (io/resource (get ctx var-name)))))
                                            ctx))
                    :extend-from (fn -extend-from [ctx path]
                                   (process (edn/read-string (slurp (io/reader (io/resource path))))
                                            ctx))
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
