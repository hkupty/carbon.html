(ns carbon.syntax
  (:require [clojure.edn :as edn]
            [carbon.debug :as debug]
            [carbon.tags :as tags]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.walk :refer [postwalk]]
            [carbon.util :as util])
  (:import (java.io File PushbackReader)))

(def ^:private search-folders (atom []))

;; there might be dragons in using concat here
(defn add-to-search-folders! [& lst] (swap! search-folders concat lst))

(defn find-template [template-name]
  (first
    (eduction
      (map (fn [obj] (cond-> obj string? (io/file))))
      (mapcat file-seq)
      (filter (fn [^File file] (.isFile file)))
      (filter (fn [^File file]
                (= (.getName file) template-name)))
      @search-folders)))

(defn read-resource [resource]
  (try
    (edn/read (PushbackReader. (io/reader (find-template resource))))
    (catch Exception ex
      (throw (ex-info "Resource not found" {:resource resource} ex)))))

(defn replacer [sym-map]
  (fn -step [expr]
    (postwalk
      (fn -replace [i]
        (if (symbol? i)
          (get sym-map i i)
          i))
      expr)))

(defn value-bind [-key -path]
  (cond
    (vector? -path) (tags/zoom tags/*ctx* -path (:default (meta -path) -key))
    (keyword? -path) (tags/zoom tags/*ctx* -path (:default (meta -path) -key))
    :else -path))

(defn normalize-bindings [binds]
  (into {}
        (map (fn [[-key -path]] [-key (value-bind -key -path)]))
        (apply hash-map binds)))

(defn bind-impl [sym-map exprs] (into [] (map (replacer sym-map)) exprs))

;; Bind: Binds one or more symbols to an expression.
;; Such expression will be evaluated.
;; Remaining expressions will have replaced the symbol by the value of the expression
(defmulti carbon-bind (fn [tag & _] tag))
(defmethod carbon-bind :default [_ ctx & args] (vec args))

;; Declares a component for future use.
;; Pre-binds values to be used in component, which will be replaced by
;; the supplied (default) value if not overwritten by the caller
;; i.e. [:c/declare [message "Hello world"] [:div [:p message]]]
(defmethod carbon-bind :c/declare [_ binds & exprs]
  (let [sym-map (into {}
                      (comp
                        (map vec)
                        (filter (comp symbol? first)))
                      (partition 2 binds))]
    (cond->> exprs (seq sym-map) (bind-impl sym-map))))

(defn- cartesian-product [sym-map]
  (let [-ks (keys sym-map)
        -vs (map (fn [-k] (get sym-map -k)) -ks)]
    (into []
          (map (fn [-cp] (into {} (map vector -ks -cp))))
          (apply combo/cartesian-product -vs))))

(defmethod carbon-bind :c/for [_ binds & exprs]
  (let [syms (cartesian-product (normalize-bindings binds))]
    (transduce
        (mapcat (fn [sym-map] (bind-impl sym-map exprs)))
        conj
        []
        syms)))

;; Declares variables to be used inside the `exprs` block
;; The values in the binding block are expected to be a path to getting
;; the real values from the context
;; i.e. [:c/let [message [:user :message]] [:div [:p message]]]
;; The values can have defaults applied in case context doesn't find a value
;; i.e. [:c/let [message ^{:default "some default value"} [:missing-key]] [:div [:p message]]]
(defmethod carbon-bind :c/let[_ binds & exprs] (bind-impl (normalize-bindings binds) exprs))

;; Applies a component into this section, replacing the content of the variables
;; declared in it with the ones supplied as `binds` in this block.
;; see :c/declare for default values
(defmethod carbon-bind :c/component [_ binds component]
  (bind-impl (normalize-bindings binds)
                    (read-resource (cond-> component (keyword? component) (-> (name) (str ".edn"))))))

(defmulti carbon-cond (fn [tag & _] tag))
(defmethod carbon-cond :default [_ ctx & args] (vec args))

(defmethod carbon-cond :c/if -if [_ condition true-branch false-branch]
  (if (true? condition)
    true-branch
    false-branch))

(defmethod carbon-cond :c/when -when [_ condition branch]
  (when (true? condition)
           branch))

