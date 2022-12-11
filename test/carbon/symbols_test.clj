(ns carbon.symbols-test
  (:require [clojure.test :refer [deftest testing is]]
            [matcher-combinators.test] ;; adds support for `match?` and `thrown-match?` in `is` expressions
            [matcher-combinators.matchers :as m]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [carbon.symbols :as symbols]))

(deftest carbon-symbols
  (testing "We can search symbols in a kw map"
    (is (match? true
                (symbols/process-symbol 'x {:x true}))))

  (testing "We can search nested symbols in a kw map"
    (is (match? true (symbols/process-symbol 'x.y/z {:x {:y {:z true}}}))))

  (testing "We can match namespaced symbols to namespaced values in a kw map"
    (is (match? true (symbols/process-symbol 'x.y/z {:x {:y/z true}})))))
