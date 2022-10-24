(ns carbon.html-test
  (:require [clojure.test :refer [deftest testing is]]
            [carbon.processor :as p]
            [matcher-combinators.test] ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.matchers :as m]
            [carbon.debug :as d]
            [carbon.tags :as tags]
            [carbon.syntax :as syntax]))

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
                          :mult [10]}))))

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
                          {:is-it-true false})))))




