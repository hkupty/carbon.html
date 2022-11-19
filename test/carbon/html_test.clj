(ns carbon.html-test
  (:require [clojure.test :refer [deftest testing is]]
            [carbon.processor :as p]
            [clojure.java.io :as io]
            [matcher-combinators.test] ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.matchers :as m]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [carbon.debug :as d]
            [carbon.tags :as tags]
            [carbon.syntax :as syntax])
  (:import (java.io File)))

(deftest carbon-syntax
  (testing :c/for
    (is (match? [:div [:p 1] [:p 2] [:p 3]]
                (p/render '[:div [:c/for [i [:items]] [:p i]]]
                          {:items [1 2 3]})))

    (is (match? [:div [:p 10] [:p 3] [:p 20] [:p 6]]
                (p/render '[:div [:c/for [base [:items]
                                          m [:mult]]
                                  [:p [* base m]]]]
                          {:items [1 2]
                           :mult [10 3]})))

    (is (match? [:div [:p 10] [:p 20]]
                (p/render '[:div [:c/for [base [:items]
                                          m [:mult]]
                                  [:p [* base m]]]]
                          {:items [1 2]
                           :mult [10]})))
    (testing "getting nested data"
      (is (match? [:div [:p "one"] [:p "two"]]
                  (p/render '[:div
                              [:c/for [data [:items :options]]
                               [:p data]]]
                            {:items {:options ["one" "two"]}})))))

  (testing :c/let
    ;; TODO Investigate if it can be unwrapped
    (is (match? [[:div "xxx"]]
                (p/render '[:c/let [-k [:key]]
                            [:div -k]]
                          {:key "xxx"})))

    (is (match? [:div
                 [:div "xxx"]
                 [:div "extra-xxx"]]
                (p/render '[:div [:c/let [-k [:key]]
                                  [:div -k]
                                  [:div [str "extra-" -k]]]]
                          {:key "xxx"})))
    (testing "^{:default xxx}"
      (is (match? [:div [:p "default text"]]
                  (p/render '[:div [:c/let [text ^{:default "default text"} [:missing-key]]
                                    [:p text]]]
                            {})))))

  (testing :c/if
    (is (match? [:div [:p "true branch"]]
                (p/render '[:div [:c/if [true? [:c/get :is-it-true]]
                                  [:p "true branch"]
                                  [:p "false branch"]]]
                          {:is-it-true true})))

    (is (match? [:div [:p "false branch"]]
                (p/render '[:div [:c/if [true? [:c/get :is-it-true]]
                                  [:p "true branch"]
                                  [:p "false branch"]]]
                          {:is-it-true false}))))

  (testing :c/when
    (is (match? [:div [:p "true branch"]]
                (p/render '[:div [:c/when [true? [:c/get :is-it-true]]
                                  [:p "true branch"]]]
                          {:is-it-true true})))

    (is (match? [:div]
                (p/render '[:div [:c/when [true? [:c/get :is-it-true]]
                                  [:p "true branch"]]]
                          {:is-it-true false}))))
  (testing :c/declare
    (let [temp (File/createTempFile "template" ".edn")
          fname (keyword (first (str/split (.getName temp) #"\.")))]
      (spit (io/output-stream temp) (pr-str '[:c/declare [base "value"] [:p base]]))
      (syntax/add-to-search-folders! (.getParentFile temp))

      (is (match? [:div [:p "Amazing"]]
                  (p/render [:div [:c/component '[base [:base]] fname]]
                            {:base "Amazing"})))

      (.delete temp)))

  (testing "attribute metadata"
    (testing "empty metadata"
      (is (match? [:p {} "text"] (p/render '[:p {} "text"] {}))))

    (testing "static metadata"
      (is (match? [:p {:x 1} "text"] (p/render '[:p {:x 1} "text"] {}))))

    (testing "dynamic metadata"
      (is (match? [:p {:x "true"} "text"]
                  (p/render '[:p
                              {:x [:c/if [:c/get :data] "true" "false"]}
                              "text"]
                            {:data true})))
      (is (match? [:p {:x "something"} "text"]
                  (p/render '[:p [:c/when [:c/get :data]
                                  {:x "something"}]
                              "text"]
                            {:data true})))

      (is (match? [:p {:x "something"} "text"]
                  (p/render '[:p [:c/when [:c/get :data]
                                  [:c/merge {:x "discard"} {:x "something"}]]
                              "text"]
                            {:data true})))

      (is (match? [:p {:x "something"} "text"]
                  (p/render '[:p [:c/when [:c/get :data]
                                  [:c/kv [:nested :x]]]
                              "text"]
                            {:data true
                             :nested {:x "something"}})))))
  (testing :c/get
    (testing "simple get"
      (is (match? [:p "text"] (p/render '[:p [:c/get :text]] {:text "text"})))))

  (testing :c/zoom
    (testing "inside for"
      (is (match? [:div
                   [:p "one"]
                   [:p "two"]]
                  (p/render '[:div
                              [:c/for [block [:data]]
                               [:p [:c/zoom block :text]]]]
                            {:data [{:text "one"} {:text "two"}]})))))

  (testing :c/slug
    (is (match? [:p {:id "the_id"} "xx"] (p/render '[:p {:id [:c/slug :id]} "xx"] {:id "the id"}))))
  )

