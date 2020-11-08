(ns cljsearch.searchfile-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljsearch.searchfile :only (new-search-file search-file-path)]))

(deftest test-searchfile-abs-path
  (let [filepath "~/src/xsearch/clojure/cljsearch/src/cljsearch/searchfile.clj"
        searchfile (new-search-file (file filepath) :code)]
    (testing "test-searchfile-abs-path"
             (is (= (search-file-path searchfile) filepath)))))

(deftest test-searchfile-rel-path-1
  (let [filepath "./searchfile.clj"
        searchfile (new-search-file (file filepath) :code)]
    (testing "test-searchfile-rel-path-1"
             (is (= (search-file-path searchfile) filepath)))))

(deftest test-searchfile-rel-path-2
  (let [filepath "../searchfile.clj"
        searchfile (new-search-file (file filepath) :code)]
    (testing "test-searchfile-rel-path-2"
             (is (= (search-file-path searchfile) filepath)))))
