(ns cljsearch.searcher-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljsearch.config :only (SHAREDPATH)]
        [cljsearch.fileutil :only (expand-path)]
        [cljsearch.searcher :only
          (filter-file? is-archive-search-file? is-search-dir? is-search-file?
           search-lines search-multiline-string)]
        [cljsearch.searchsettings :only
          (DEFAULT-SETTINGS add-extension add-pattern set-archives-only)]))

(def TESTFILE
  (str/join java.io.File/separator [SHAREDPATH "testFiles" "testFile2.txt"]))

(defn get-settings []
  (add-pattern DEFAULT-SETTINGS "Searcher" :search-patterns))

;; *****************************************************************************
;; is-search-dir? tests
;; *****************************************************************************
(deftest test-is-search-dir?-default-settings
  (testing "test-is-search-dir?-default-settings"
    (is (is-search-dir? (file ".") DEFAULT-SETTINGS))
    (is (is-search-dir? (file "..") DEFAULT-SETTINGS))
    (is (not (is-search-dir? (file ".git") DEFAULT-SETTINGS)))
    (is (is-search-dir? (file "clojure") DEFAULT-SETTINGS))))

(deftest test-is-search-dir?-with-in-dir-patterns
  (let [settings (add-pattern DEFAULT-SETTINGS "search" :in-dir-patterns)]
    (testing "test-is-search-dir?-with-in-dir-patterns"
      (is (is-search-dir? (file "cljsearch") settings))
      (is (is-search-dir? (file "searcher") settings))
      (is (not (is-search-dir? (file "clojure") settings))))))

(deftest test-is-search-dir?-with-out-dir-patterns
  (let [settings (add-pattern DEFAULT-SETTINGS "clojure" :out-dir-patterns)]
    (testing "test-is-search-dir?-with-out-dir-patterns"
      (is (is-search-dir? (file "cljsearch") settings))
      (is (is-search-dir? (file "searcher") settings))
      (is (not (is-search-dir? (file "clojure") settings))))))

(deftest test-is-search-dir?-with-include-hidden
  (let [settings (assoc DEFAULT-SETTINGS :exclude-hidden false)]
    (testing "test-is-search-dir?-with-include-hidden"
    (is (is-search-dir? (file ".") settings))
    (is (is-search-dir? (file "..") settings))
    (is (is-search-dir? (file ".git") settings))
    (is (is-search-dir? (file "clojure") settings)))))

;; *****************************************************************************
;; is-search-file? tests
;; *****************************************************************************
(deftest test-is-search-file?-default-settings
  (testing "test-is-search-file?-default-settings"
    (is (is-search-file? (file "searcher.clj") DEFAULT-SETTINGS))))

(deftest test-is-search-file?-with-in-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "clj,js" :in-extensions)]
    (testing "test-is-search-file?-with-in-extensions"
      (is (is-search-file? (file "searcher.clj") settings))
      (is (is-search-file? (file "searcher.js") settings))
      (is (not (is-search-file? (file "searcher.py") settings))))))

(deftest test-is-search-file?-with-out-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "py" :out-extensions)]
    (testing "test-is-search-dir?-with-out-extensions"
      (is (is-search-file? (file "searcher.clj") settings))
      (is (is-search-file? (file "searcher.js") settings))
      (is (not (is-search-file? (file "searcher.py") settings))))))

(deftest test-is-search-file?-with-in-file-patterns
  (let [settings (add-pattern DEFAULT-SETTINGS "search" :in-file-patterns)]
    (testing "test-is-search-file?-with-in-file-patterns"
      (is (is-search-file? (file "cljsearch.clj") settings))
      (is (is-search-file? (file "searcher.clj") settings))
      (is (not (is-search-file? (file "fileutil.clj") settings))))))

(deftest test-is-search-file?-with-out-file-patterns
  (let [settings (add-pattern DEFAULT-SETTINGS "search" :out-file-patterns)]
    (testing "test-is-search-dir?-with-out-file-patterns"
      (is (is-search-file? (file "fileutil.clj") settings))
      (is (not (is-search-file? (file "cljsearch.clj") settings)))
      (is (not (is-search-file? (file "searcher.clj") settings))))))

;; *****************************************************************************
;; is-archive-search-file? tests
;; *****************************************************************************
(deftest test-is-archive-search-file?-default-settings
  (testing "test-is-archive-search-file?-default-settings"
    (is (is-archive-search-file? (file "archive.zip") DEFAULT-SETTINGS))))

(deftest test-iis-archive-search-file?-with-in-earchivextensions
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archive-extensions)]
    (testing "test-is-archive-search-file?-with-in-archive-extensions"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "archive.bz2") settings))
      (is (not (is-archive-search-file? (file "archive.gz") settings))))))

(deftest test-is-archive-search-file?-with-out-archive-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "gz" :out-archive-extensions)]
    (testing "test-is-archive-search-file?-with-out-archive-extensions"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "archive.bz2") settings))
      (is (not (is-archive-search-file? (file "archive.gz") settings))))))

(deftest test-is-archive-search-file?-with-in-archive-file-patterns
  (let [settings (add-pattern DEFAULT-SETTINGS "arch" :in-archive-file-patterns)]
    (testing "test-is-archive-search-file?-with-in-archive-file-patterns"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "arch.bz2") settings))
      (is (not (is-archive-search-file? (file "compressed.gz") settings))))))

(deftest test-is-archive-search-file?-with-out-archive-file-patterns
  (let [settings (add-pattern DEFAULT-SETTINGS "compress" :out-archive-file-patterns)]
    (testing "test-is-archive-search-file?-with-out-archive-file-patterns"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "arch.bz2") settings))
      (is (not (is-archive-search-file? (file "compressed.gz") settings))))))

;; *****************************************************************************
;; filter-file? tests
;; *****************************************************************************
(deftest test-filter-file?-default-settings
  (testing "test-filter-file?-default-settings"
    (is (filter-file? (file "searcher.clj") DEFAULT-SETTINGS))))

(deftest test-filter-file?-with-search-file-settings
  (let [settings (add-extension DEFAULT-SETTINGS "clj,js" :in-extensions)]
    (testing "test-filter-file?-with-search-file-settings"
      (is (filter-file? (file "searcher.clj") settings))
      (is (filter-file? (file "searcher.js") settings))
      (is (not (filter-file? (file "searcher.py") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-archive-search-file-settings
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archive-extensions)]
    (testing "test-filter-file?-with-archive-search-file-settings"
      (is (not (filter-file? (file "archive.zip") settings)))
      (is (not (filter-file? (file "archive.bz2") settings)))
      (is (not (filter-file? (file "archive.gz") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-search-archives
  (let [settings (assoc DEFAULT-SETTINGS :search-archives true)]
    (testing "test-filter-file?-with-search-archives"
      (is (filter-file? (file "archive.zip") settings))
      (is (filter-file? (file "archive.bz2") settings))
      (is (filter-file? (file "archive.gz") settings))
      (is (filter-file? (file "searcher.clj") settings))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-archives-only
  (let [settings (set-archives-only DEFAULT-SETTINGS true)]
    (testing "test-filter-file?-with-archives-only"
      (is (filter-file? (file "archive.zip") settings))
      (is (filter-file? (file "archive.bz2") settings))
      (is (filter-file? (file "archive.gz") settings))
      (is (not (filter-file? (file "searcher.clj") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-includehidden
  (let [settings (assoc DEFAULT-SETTINGS :exclude-hidden false)]
    (testing "test-filter-file?-with-includehidden"
      (is (filter-file? (file "searcher.clj") settings))
      (is (filter-file? (file ".gitignore") settings))
      (is (not (filter-file? (file "archive.zip") settings))))))

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

