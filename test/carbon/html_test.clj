(ns carbon.html-test
  (:require [clojure.test :refer [deftest testing is]]
            [carbon.processor :as p]
            [clojure.java.io :as io]
            [matcher-combinators.test] ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.matchers :as m]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [carbon.tags :as tags]
            [carbon.syntax :as syntax])
  (:import (java.io File)
           (java.time Instant)
           (java.time.temporal ChronoField)))

(deftest carbon-v2-syntax
  (testing :for
    (is (match? [:div [:p 1] [:p 2] [:p 3]]
                (p/render '[:div [:for [i items] [:p i]]]
                         {:items [1 2 3]}
                         syntax/default-tags))))


  (testing :if
    (is (match? [:div [:p "true branch"]]
                  (p/render '[:div [:if is-it-true
                                   [:p "true branch"]
                                   [:p "false branch"]]]
                           {:is-it-true true}
                           syntax/default-tags)))

   (is (match? [:div [:p "false branch"]]
                  (p/render '[:div [:if is-it-true
                                   [:p "true branch"]
                                   [:p "false branch"]]]
                           {:is-it-true false}
                           syntax/default-tags))))

 (testing :when
    (is (match? [:div [:p "true branch"]]
                  (p/render '[:div [:when is-it-true
                                   [:p "true branch"]]]
                           {:is-it-true true}
                           syntax/default-tags)))

    (is (match? [:div]
                (p/render '[:div [:when is-it-true
                                  [:p "true branch"]]]
                          {:is-it-true false}
                          syntax/default-tags))))
(testing :component
   (testing "simple component"
     (is (match? [:div [:p "Amazing"]]
               (p/render '[:div [:Component argument]]
                         {:argument "Amazing"}
                         {:Component (tags/component ['base] [:p 'base])}))))

   (testing "custom function"
     (is (match? [:p 150]
                   (p/render [:calc 30]
                             {}
                             {:calc
                              (fn [[arg] _base-map _components]
                                [:p (+ 120 arg)])})))))

 (testing "attribute metadata"
    (testing "empty metadata"
      (is (match? [:p {} "text"] (p/render '[:p {} "text"] {} syntax/default-tags))))

  (testing "static metadata"
      (is (match? [:p {:x 1} "text"] (p/render '[:p {:x 1} "text"] {} syntax/default-tags))))

  (testing "dynamic metadata"
    (is (match? [:p {:x "true"} "text"]
                (p/render '[:p
                            {:x [:if data "true" "false"]}
                            "text"]
                          {:data true}
                          syntax/default-tags)))

    (is (match? [:p {:x "something"} "text"]
                (p/render '[:p [:when 'data
                                {:x "something"}]
                            "text"]
                          {:data true}
                          syntax/default-tags)))

    (is (match? [:p {:x "something"} "text"]
                (p/render '[:p [:when data
                                [:merge {:x "discard"} {:x "something"}]]
                            "text"]
                          {:data true}
                          syntax/default-tags)))

    (is (match? [:p {:x "something"} "text"]
                (p/render '[:p [:when data
                                nested]
                            "text"]
                          {:data true
                           :nested {:x "something"}}
                          syntax/default-tags)))
    (is (match? [:p "text"]
                (p/render '[:p [:when ^:nullable? data
                                "text"]]
                          {:data true}
                          syntax/default-tags)))
    (is (match? [:p]
                (p/render '[:p [:when ^:nullable? data
                                "text"]]
                          {}
                          syntax/default-tags))))))
