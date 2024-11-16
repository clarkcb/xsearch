(ns cljsearch.searcher-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljsearch.config :only (SHAREDPATH)]
        [cljfind.fileutil :only (expand-path)]
        [cljsearch.searcher :only
          (search-lines search-multiline-string)]
        [cljfind.findsettings]
        [cljsearch.searchsettings :only
         (DEFAULT-SEARCH-SETTINGS)]))

(def TESTFILE
  (str/join java.io.File/separator [SHAREDPATH "testFiles" "testFile2.txt"]))

(defn get-settings []
  (add-pattern DEFAULT-SEARCH-SETTINGS "Searcher" :search-patterns))

;; *****************************************************************************
;; search-lines tests
;; *****************************************************************************
(deftest test-search-lines
  (testing "test-search-lines"
    (with-open [rdr (reader (expand-path TESTFILE))]
      (let [settings (get-settings)
            results (search-lines (line-seq rdr) settings)]
        (is (= (count results) 2))
        (is (= (:line-num (first results)) 29))
        (is (= (:matchstartindex (first results)) 3))
        (is (= (:matchendindex (first results)) 11))
        (is (= (:line-num (second results)) 35))
        (is (= (:matchstartindex (second results)) 24))
        (is (= (:matchendindex (second results)) 32))))))

;; *****************************************************************************
;; search-multiline-string tests
;; *****************************************************************************
(deftest test-search-multiline-string
  (testing "test-search-multiline-string"
    (let [settings (get-settings)
          contents (slurp (expand-path TESTFILE))
          results (search-multiline-string contents settings)]
      (is (= (count results) 2))
      (is (= (:line-num (first results)) 29))
      (is (= (:matchstartindex (first results)) 3))
      (is (= (:matchendindex (first results)) 11))
      (is (= (:line-num (second results)) 35))
      (is (= (:matchstartindex (second results)) 24))
      (is (= (:matchendindex (second results)) 32)))))
