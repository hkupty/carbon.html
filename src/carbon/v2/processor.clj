(ns carbon.v2.processor
  (:require [carbon.v2.tags :as tags-v2]
            [carbon.debug :as debug]
            [carbon.v2.syntax :as syntax-v2]
            [hiccup.page :refer [doctype]]
            [hiccup.core :refer [html]]))

;; Low-level API
(defn render [tree context components]
  (tags-v2/mount tree context components))

;; High-level API
(defn render-page [tree context components]
  (html {:mode :html}
        (doctype :html5)
        (render tree context (merge components syntax-v2/default-tags))))
