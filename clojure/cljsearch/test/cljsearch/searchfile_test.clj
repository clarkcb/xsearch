(ns cljsearch.search-file-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljsearch.searchfile :only (new-search-file search-file-path)]))

(deftest test-search-file-abs-path
  (let [file-path "~/src/xsearch/clojure/cljsearch/src/cljsearch/searchfile.clj"
        search-file (new-search-file (file file-path) :code)]
    (testing "test-search-file-abs-path"
             (is (= (search-file-path search-file) file-path)))))

(deftest test-search-file-rel-path-1
  (let [file-path "./searchfile.clj"
        search-file (new-search-file (file file-path) :code)]
    (testing "test-search-file-rel-path-1"
             (is (= (search-file-path search-file) file-path)))))

(deftest test-search-file-rel-path-2
  (let [file-path "../searchfile.clj"
        search-file (new-search-file (file file-path) :code)]
    (testing "test-search-file-rel-path-2"
             (is (= (search-file-path search-file) file-path)))))
