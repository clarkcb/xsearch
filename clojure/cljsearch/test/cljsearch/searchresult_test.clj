(ns cljsearch.searchresult-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (trim)])
  (:use [clojure.java.io :only (file)])
  (:use [cljsearch.config :only (XSEARCHPATH)])
  (:use [cljsearch.searchresult :only (->SearchResult search-result-to-string)])
  (:use [cljsearch.searchsettings :only (DEFAULT-SETTINGS)])
  (:use [cljsearch.color :only (RESET GREEN)]))

(def CSSEARCHPATH
  (str XSEARCHPATH "/csharp/CsSearch/CsSearch"))

(deftest test-singleline-searchresult
  (testing "test-singleline-searchresult"
    (let [settings (assoc DEFAULT-SETTINGS :colorize false)
          searchfile (file (str CSSEARCHPATH "/Searcher.cs"))
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
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-singleline-longer-than-maxlength-searchresult
  (testing "test-singleline-longer-than-maxlength-searchresult"
    (let [settings (assoc DEFAULT-SETTINGS :colorize false :maxlinelength 100)
          searchfile (file "./maxlen.txt")
          linenum 1
          matchstartindex 53
          matchendindex 59
          line "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
          linesbefore []
          linesafter []
          r (->SearchResult
              (re-pattern "maxlen")
              searchfile
              linenum
              matchstartindex
              matchendindex
              line
              linesbefore
              linesafter)
          expected-line "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
          expected (format "%s: %d: [%d:%d]: %s" searchfile linenum
            matchstartindex matchendindex expected-line)]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-singleline-colorize-searchresult
  (testing "test-singleline-longer-than-maxlength-searchresult"
    (let [settings (assoc DEFAULT-SETTINGS :colorize true :maxlinelength 100)
          searchfile (file "./maxlen.txt")
          linenum 1
          matchstartindex 53
          matchendindex 59
          line "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
          linesbefore []
          linesafter []
          r (->SearchResult
              (re-pattern "maxlen")
              searchfile
              linenum
              matchstartindex
              matchendindex
              line
              linesbefore
              linesafter)
          expected-line (str "...89012345678901234567890123456789012345678901" GREEN "maxlen" RESET "89012345678901234567890123456789012345678901...")
          expected (format "%s: %d: [%d:%d]: %s" searchfile linenum
            matchstartindex matchendindex expected-line)]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-binaryfile-searchresult
  (testing "test-binaryfile-searchresult"
    (let [settings DEFAULT-SETTINGS
          searchfile (file (str CSSEARCHPATH "/Searcher.exe"))
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
          expected (format "%s matches at [%d:%d]" searchfile matchstartindex
            matchendindex)]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-multiline-searchresult
  (testing "test-multiline-searchresult"
    (let [settings DEFAULT-SETTINGS
          searchfile (file (str CSSEARCHPATH "/Searcher.cs"))
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
      (is (= (search-result-to-string r settings) expected)))))
