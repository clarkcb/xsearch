(ns cljsearch.search-result-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (trim)]
        [clojure.java.io :only (file)]
        [cljsearch.config :only (XSEARCHPATH)]
        [cljsearch.searchfile :only (new-search-file search-file-path)]
        [cljsearch.searchresult :only (->SearchResult search-result-to-string)]
        [cljsearch.searchsettings :only (DEFAULT-SETTINGS)]
        [cljsearch.color :only (RESET GREEN)]))

(def CSSEARCHPATH
  (str XSEARCHPATH "/csharp/CsSearch/CsSearch"))

(deftest test-singleline-search-result
  (testing "test-singleline-search-result"
    (let [settings (assoc DEFAULT-SETTINGS :colorize false)
          file (file (str CSSEARCHPATH "/Searcher.cs"))
          search-file (new-search-file file :code)
          line-num 10
          match-start-index 15
          match-end-index 23
          line "\tpublic class Searcher\n"
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "Search")
              search-file 
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected (format "%s: %d: [%d:%d]: %s" (search-file-path search-file) line-num
            match-start-index match-end-index (str/trim line))]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-singleline-longer-than-maxlength-search-result
  (testing "test-singleline-longer-than-maxlength-search-result"
    (let [settings (assoc DEFAULT-SETTINGS :colorize false :max-line-length 100)
          file (file "./maxlen.txt")
          search-file (new-search-file file :text)
          line-num 1
          match-start-index 53
          match-end-index 59
          line "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "maxlen")
              search-file
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected-line "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
          expected (format "%s: %d: [%d:%d]: %s" (search-file-path search-file) line-num
            match-start-index match-end-index expected-line)]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-singleline-colorize-search-result
  (testing "test-singleline-longer-than-maxlength-search-result"
    (let [settings (assoc DEFAULT-SETTINGS :colorize true :max-line-length 100)
          file (file "./maxlen.txt")
          search-file (new-search-file file :text)
          line-num 1
          match-start-index 53
          match-end-index 59
          line "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "maxlen")
              search-file
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected-line (str "...89012345678901234567890123456789012345678901" GREEN "maxlen" RESET "89012345678901234567890123456789012345678901...")
          expected (format "%s: %d: [%d:%d]: %s" (search-file-path search-file) line-num
            match-start-index match-end-index expected-line)]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-binaryfile-search-result
  (testing "test-binaryfile-search-result"
    (let [settings DEFAULT-SETTINGS
          file (file (str CSSEARCHPATH "/Searcher.exe"))
          search-file (new-search-file file :binary)
          line-num 0
          match-start-index 0
          match-end-index 0
          line nil
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "Search")
              search-file 
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected (format "%s matches at [%d:%d]" (search-file-path search-file) match-start-index
            match-end-index)]
      (is (= (search-result-to-string r settings) expected)))))

(deftest test-multiline-search-result
  (testing "test-multiline-search-result"
    (let [settings DEFAULT-SETTINGS
          file (file (str CSSEARCHPATH "/Searcher.cs"))
          search-file (new-search-file file :binary)
          line-num 10
          match-start-index 15
          match-end-index 23
          line "\tpublic class Searcher\n"
          lines-before ["namespace CsSearch\n" "{\n"]
          lines-after ["\t{\n" "\t\tprivate readonly FileTypes _fileTypes;\n"]
          r (->SearchResult
              (re-pattern "Search")
              search-file 
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
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
          expected (format outputformat (search-file-path search-file) line-num
            match-start-index match-end-index)]
      (is (= (search-result-to-string r settings) expected)))))
