(ns cljsearch.searchresult-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as str :only (trim)])
  (:use [clojure.java.io :only (file)])
  (:use [cljsearch.searchresult]))

(deftest test-singleline-searchresult
  (testing "test-singleline-searchresult"
    (let [searchfile (file "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs")
          linenum 10
          matchstartindex 15
          matchendindex 23
          line "\tpublic class Searcher\n"
          linesbefore []
          linesafter []
          r (->SearchResult
              (re-pattern "Search")
              searchfile 
              linenum
              matchstartindex
              matchendindex
              line
              linesbefore
              linesafter)
          expected (format "%s: %d: [%d:%d]: %s" searchfile linenum
            matchstartindex matchendindex (str/trim line))]
      (is (= (search-result-to-string r) expected)))))

(deftest test-binaryfile-searchresult
  (testing "test-binaryfile-searchresult"
    (let [searchfile (file "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.exe")
          linenum 0
          matchstartindex 0
          matchendindex 0
          line nil
          linesbefore []
          linesafter []
          r (->SearchResult
              (re-pattern "Search")
              searchfile 
              linenum
              matchstartindex
              matchendindex
              line
              linesbefore
              linesafter)
          expected (format "%s matches" searchfile)]
      (is (= (search-result-to-string r) expected)))))

(deftest test-multiline-searchresult
  (testing "test-multiline-searchresult"
    (let [searchfile (file "~/src/git/xsearch/csharp/CsSearch/CsSearch/Searcher.cs")
          linenum 10
          matchstartindex 15
          matchendindex 23
          line "\tpublic class Searcher\n"
          linesbefore ["namespace CsSearch\n" "{\n"]
          linesafter ["\t{\n" "\t\tprivate readonly FileTypes _fileTypes;\n"]
          r (->SearchResult
              (re-pattern "Search")
              searchfile 
              linenum
              matchstartindex
              matchendindex
              line
              linesbefore
              linesafter)
          outputformat
"================================================================================
%s: %d: [%d:%d]
--------------------------------------------------------------------------------
   8 | namespace CsSearch
   9 | {
> 10 | \tpublic class Searcher
  11 | \t{
  12 | \t\tprivate readonly FileTypes _fileTypes;
"
          expected (format outputformat searchfile linenum
            matchstartindex matchendindex)]
      (is (= (search-result-to-string r) expected)))))

