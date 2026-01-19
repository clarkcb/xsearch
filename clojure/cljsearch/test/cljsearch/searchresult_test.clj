(ns cljsearch.searchresult-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (trim)]
        [clojure.java.io :only (file)]
        [cljsearch.config :only (XSEARCHPATH)]
        [cljfind.consolecolor]
        [cljfind.fileresult :only (new-file-result file-result-path)]
        [cljfind.fileutil :only (get-path-name to-path)]
        [cljsearch.searchresult :only (->SearchResult get-search-result-formatter)]
        [cljsearch.searchsettings :only (DEFAULT-SEARCH-SETTINGS)]))

(def CSSEARCHPATH
  (str XSEARCHPATH "/csharp/CsSearch/CsSearch"))

(deftest test-singleline-search-result
  (testing "test-singleline-search-result"
    (let [settings (assoc DEFAULT-SEARCH-SETTINGS :colorize false)
          file (to-path (str CSSEARCHPATH "/Searcher.cs"))
          file-result (new-file-result file :code)
          line-num 10
          match-start-index 15
          match-end-index 23
          line "\tpublic class Searcher\n"
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "Search")
              file-result
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected (format "%s: %d: [%d:%d]: %s" (file-result-path file-result) line-num
            match-start-index match-end-index (str/trim line))
          format-search-result (get-search-result-formatter settings)]
      (is (= (format-search-result r) expected)))))

(deftest test-singleline-longer-than-maxlength-search-result
  (testing "test-singleline-longer-than-maxlength-search-result"
    (let [settings (assoc DEFAULT-SEARCH-SETTINGS :colorize false :max-line-length 100)
          file (to-path "./maxlen.txt")
          file-result (new-file-result file :text)
          line-num 1
          match-start-index 53
          match-end-index 59
          line "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "maxlen")
              file-result
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected-line "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
          expected (format "%s: %d: [%d:%d]: %s" (file-result-path file-result) line-num
            match-start-index match-end-index expected-line)
          format-search-result (get-search-result-formatter settings)]
      (is (= (format-search-result r) expected)))))

(deftest test-singleline-colorize-search-result
  (testing "test-singleline-longer-than-maxlength-search-result"
    (let [settings (assoc DEFAULT-SEARCH-SETTINGS :colorize true :max-line-length 100)
          file (to-path "./maxlen.txt")
          file-result (new-file-result file :text)
          line-num 1
          match-start-index 53
          match-end-index 59
          line "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "maxlen")
              file-result
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected-line (str "...89012345678901234567890123456789012345678901" CONSOLE_COLOR_GREEN "maxlen" CONSOLE_COLOR_RESET "89012345678901234567890123456789012345678901...")
          expected (format "%s: %d: [%d:%d]: %s" (file-result-path file-result) line-num
            match-start-index match-end-index expected-line)
          format-search-result (get-search-result-formatter settings)]
      (is (= (format-search-result r) expected)))))

(deftest test-binaryfile-search-result
  (testing "test-binaryfile-search-result"
    (let [settings DEFAULT-SEARCH-SETTINGS
          file (to-path (str CSSEARCHPATH "/Searcher.exe"))
          file-result (new-file-result file :binary)
          line-num 0
          match-start-index 0
          match-end-index 0
          line nil
          lines-before []
          lines-after []
          r (->SearchResult
              (re-pattern "Search")
              file-result
              line-num
              match-start-index
              match-end-index
              line
              lines-before
              lines-after)
          expected (format "%s matches at [%d:%d]" (file-result-path file-result) match-start-index
            match-end-index)
          format-search-result (get-search-result-formatter settings)]
      (is (= (format-search-result r) expected)))))

(deftest test-multiline-search-result
  (testing "test-multiline-search-result"
    (let [settings DEFAULT-SEARCH-SETTINGS
          file (to-path (str CSSEARCHPATH "/Searcher.cs"))
          file-result (new-file-result file :code)
          line-num 10
          match-start-index 15
          match-end-index 23
          line "\tpublic class Searcher\n"
          lines-before ["namespace CsSearch\n" "{\n"]
          lines-after ["\t{\n" "\t\tprivate readonly FileTypes _fileTypes;\n"]
          r (->SearchResult
              (re-pattern "Search")
              file-result
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
          expected (format outputformat (file-result-path file-result) line-num
            match-start-index match-end-index)
          format-search-result (get-search-result-formatter settings)]
      (is (= (format-search-result r) expected)))))
