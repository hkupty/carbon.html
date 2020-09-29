(ns carbon.template
  (:require [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]
            [hiccup.page :as p]
            [hiccup2.core :as h]
            [hiccup.util :as hu]
            [clojure.java.io :as io]))

(defn read-resource [resource]
  (edn/read-string (slurp (io/reader (io/resource resource)))))

(defn namespaced-as [-ns kw]
  (and (keyword? kw)
         (= -ns (namespace kw))))

(defn run-fn [coll ctx]
  (let [[-fn & args] coll]
    (apply -fn ctx args)))

(declare process)
(declare process-argument)

(defn zoom [-map -path]
  (if (coll? -path)
    (get-in -map -path)
    (get -map -path)))

(def render-template
  #:carbon{:default (fn -default [ctx var-path templ]
                      (let [arg (zoom ctx var-path)]
                        (process-argument ctx
                                          (if (some? arg)
                                            arg
                                            templ))))
           :get (fn -get [ctx var-path]
                  (zoom ctx var-path))
           :extend-with (fn -extend-with [ctx var-name]
                          (process (read-resource (zoom ctx var-name))
                                   ctx))
           :extend-from (fn -extend-from [ctx path]
                          (process (read-resource path)
                                   ctx))
           :component (fn -component [ctx path ctx-path]
                        (process (read-resource path)
                                 (zoom ctx ctx-path)))
           :chose (fn [ctx -key options]
                    (->> -key
                        (get ctx)
                        (get options)))
           :map (fn -map [ctx tag -fn & args]
                  (let [fn-args (vec (butlast args))
                        coll (last args)]
                    (into [tag]
                          (map (fn [item]
                                 (apply -fn ctx (conj fn-args item))))
                          coll)))
           :include (fn -include [_ path]
                      (hu/raw-string (slurp path)))})


(defn process-argument [ctx arg]
  (cond
    (namespaced-as "carbon" arg) (render-template arg)
    (and (vector? arg)
         (fn? (first arg))) (run-fn arg ctx)
    (and (vector? arg)
         (= 1 (count arg))
         (list? (first arg))) (first arg)
    (vector? arg) (mapv (partial process-argument ctx) arg)
    :else arg))


(defn process [tree ctx]
  (postwalk (partial process-argument ctx)
    tree))

(defn render [tag templ content]
  (into [tag]
       (process templ content)))

(defn apply-template [content {:keys [head body]}]
  (p/html5 {:mode :html}
           (render :head head content)
           (render :body body content)))
