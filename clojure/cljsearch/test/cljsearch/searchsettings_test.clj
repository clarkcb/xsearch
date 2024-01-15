(ns cljsearch.searchsettings-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljsearch.searchsettings :only
    (DEFAULT-SETTINGS add-extension add-pattern set-archives-only set-debug)]))

(deftest test-default-settings
  (let [settings DEFAULT-SETTINGS]
    (testing "test-default-settings"
      (is (not (:archives-only settings)))
      (is (not (:debug settings)))
      (is (not (:first-match settings)))
      (is (not (:include-hidden settings)))
      (is (= (:lines-after settings) 0))
      (is (= (:lines-before settings) 0))
      (is (not (:list-dirs settings)))
      (is (not (:list-files settings)))
      (is (not (:list-lines settings)))
      (is (= (:max-line-length settings) 150))
      (is (not (:multi-line-search settings)))
      (is (:print-results settings))
      (is (not (:print-usage settings)))
      (is (not (:print-version settings)))
      (is (:recursive settings))
      (is (not (:search-archives settings)))
      (is (= (:startpath settings) nil))
      (is (not (:unique-lines settings)))
      (is (not (:verbose settings))))))

(deftest test-add-extensions
  (let [settings DEFAULT-SETTINGS
        with-txt (add-extension settings "txt" :in-extensions)
        with-mult (add-extension with-txt "cs,clj" :in-extensions)]
    (testing "test-add-extensions"
      (is (= (count (:in-extensions with-txt)) 1))
      (is (= (count (:in-extensions with-mult)) 3)))))

(deftest test-add-pattern
  (let [settings DEFAULT-SETTINGS
        with-pattern (add-pattern settings "Search" :search-patterns)]
    (testing "test-add-pattern"
      (is (= (count (:search-patterns with-pattern)) 1)))))

(deftest test-set-archives-only
  (let [settings DEFAULT-SETTINGS
        with-archives-only (set-archives-only settings true)]
    (testing "test-set-archives-only"
      (is (:archives-only with-archives-only))
      (is (:search-archives with-archives-only)))))

(deftest test-set-debug
  (let [settings DEFAULT-SETTINGS
        with-debug (set-debug settings true)]
    (testing "test-set-debug"
      (is (:debug with-debug))
      (is (:verbose with-debug)))))
