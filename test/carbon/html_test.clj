(ns carbon.html-test
  (:require [clojure.test :refer [deftest testing is]]
            [carbon.processor :as p]
            [matcher-combinators.test] ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.matchers :as m]
            [carbon.debug :as d]
            [carbon.tags :as tags]
            [carbon.syntax :as syntax]))

(defn process [tree context]
  (binding [tags/*ctx* context]
    (p/process tree)))

(deftest carbon-syntax
  (testing :c/for
    (is (match? [:div [:p 1] [:p 2] [:p 3]]
                (process '[:div [:c/for [i [:items]] [:p i]]]
                         {:items [1 2 3]})))

    (is (match? [:div [:p 10] [:p 3] [:p 20] [:p 6]]
                (process '[:div [:c/for [base [:items]
                                         m [:mult]]
                                 [:p [* base m]]]]
                         {:items [1 2]
                          :mult [10 3]})))

    (is (match? [:div [:p 10] [:p 20]]
                (process '[:div [:c/for [base [:items]
                                         m [:mult]]
                                 [:p [* base m]]]]
                         {:items [1 2]
                          :mult [10]}))))

  (testing :c/let
    ;; TODO Investigate if it can be unwrapped
    (is (match? [[:div "xxx"]]
                (process '[:c/let [-k [:key]]
                           [:div -k]]
                         {:key "xxx"})))

    (is (match? [:div
                 [:div "xxx"]
                 [:div "extra-xxx"]]
                (process '[:div [:c/let [-k [:key]]
                           [:div -k]
                           [:div [str "extra-" -k]]]]
                         {:key "xxx"})))
    (testing 'defaults
      (is (match? [:div [:p "default text"]]
                  (process '[:div [:c/let [text ^{:default "default text"} [:missing-key]]
                                   [:p text]]]
                           {}))))
    ))


