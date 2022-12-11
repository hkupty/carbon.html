(ns carbon.fixtures-test
  (:require [clojure.test :refer [deftest testing is]]
            [matcher-combinators.test] ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.matchers :as m]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [carbon.tags :as tags]
            [carbon.processor :as processor]
            [carbon.util :refer [map-vals]])
  (:import (java.io File)))

(def base (io/file "fixtures/"))

(defn read-edn [^File folder ^String fname]
  (letfn [(parse [^java.io.BufferedReader reader]
            (edn/read (java.io.PushbackReader. reader)))]
    (-> folder
      (io/file fname)
      (io/reader)
      (parse))))

(defn read-components [^File folder]
  (-> folder
      (read-edn "components.edn")
      (->> (map-vals (partial apply tags/component)))))

(defn read-input [^File folder]
  (let [kv (read-edn folder "input.edn")]
    [(:tree kv) (:context kv)]))

(defn read-expected [^File folder]
  (str/trim (slurp (io/file folder "expected.html"))))

(defn run-test-for [^File folder]
  (let [components (read-components folder)
        [tree context] (read-input folder)
        expected (read-expected folder)]
    (testing (str "Test fixture: " (.getName folder))
      (is (match? expected (processor/render tree context components))))))


(deftest fixtures-test

  (run! run-test-for
        (.listFiles base)))

