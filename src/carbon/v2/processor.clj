(ns carbon.v2.processor
  (:require [carbon.v2.tags :as tags-v2]
            [carbon.debug :as debug]
            [carbon.v2.syntax :as syntax-v2]
            [clojure.string :as str]
            [hiccup.page :refer [doctype]]
            [hiccup.core :refer [html]]))

(defn flatten-exception [^clojure.lang.ExceptionInfo ex]
  (when-let [component (some-> ex ex-data :component)]
    (lazy-seq (cons component (flatten-exception (ex-cause ex))))))

(defn last-exception [^clojure.lang.ExceptionInfo ex]
  (cond-> ex (ex-cause ex) (-> (ex-cause) (recur))))

(defmacro with-flattened-error [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       (let [last-ex# (last-exception e#)
             flattened# (flatten-exception e#)]
         (println
           (ex-message e#)
           (ex-message last-ex#))
         (println (str/join " -> " flattened#))
         (println (ex-data last-ex#))
         (throw (ex-info (ex-message e#) {:components flattened#} last-ex#))))))

(defn tap [x] (println x) x)

;; Low-level API
(defn render [tree context components]
  (tags-v2/process-tree tree context components))

;; High-level API
(defn render-page [tree context components]
  (html {:mode :html}
        (doctype :html5)
        (render tree context (merge components syntax-v2/default-tags))))
