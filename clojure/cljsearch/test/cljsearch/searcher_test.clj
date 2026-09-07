(ns cljsearch.searcher-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileutil :only (expand-path)]
        [cljsearch.searchconfig :only (get-shared-path)]
        [cljsearch.searcher :only
          (create-searcher search-lines search-multiline-string)]
        [cljfind.findsettings]
        [cljsearch.searchsettings :only
         (DEFAULT-SEARCH-SETTINGS)]))

(def TESTFILE
  (str/join java.io.File/separator [ (get-shared-path) "testFiles" "testFile2.txt"]))

(defn get-settings []
  (add-pattern DEFAULT-SEARCH-SETTINGS "Searcher" :search-patterns))

;; *****************************************************************************
;; search-lines tests
;; *****************************************************************************
(deftest test-search-lines
  (testing "test-search-lines"
    (with-open [rdr (reader (expand-path TESTFILE))]
      (let [settings (get-settings)
            searcher (create-searcher settings)
            results (search-lines searcher (line-seq rdr))]
        (is (= (count results) 2))
        (is (= (:line-num (first results)) 30))
        (is (= (:matchstartindex (first results)) 3))
        (is (= (:matchendindex (first results)) 11))
        (is (= (:line-num (second results)) 36))
        (is (= (:matchstartindex (second results)) 24))
        (is (= (:matchendindex (second results)) 32))))))

;; *****************************************************************************
;; search-multiline-string tests
;; *****************************************************************************
(deftest test-search-multiline-string
  (testing "test-search-multiline-string"
    (let [settings (get-settings)
          searcher (create-searcher settings)
          contents (slurp (expand-path TESTFILE))
          results (search-multiline-string searcher contents)]
      (is (= (count results) 2))
      (is (= (:line-num (first results)) 30))
      (is (= (:matchstartindex (first results)) 3))
      (is (= (:matchendindex (first results)) 11))
      (is (= (:line-num (second results)) 36))
      (is (= (:matchstartindex (second results)) 24))
      (is (= (:matchendindex (second results)) 32)))))
