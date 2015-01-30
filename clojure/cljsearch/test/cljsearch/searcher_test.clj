(ns cljsearch.searcher-test
  (:use [clojure.java.io :only (file)])
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as str :only (join)])
  (:use [cljsearch.searcher])
  (:use [cljsearch.searchsettings]))

;; *****************************************************************************
;; is-search-dir? tests
;; *****************************************************************************
(deftest test-is-search-dir?-default-settings
  (testing "test-is-search-dir?-default-settings"
    (is (is-search-dir? (file ".") DEFAULT-SETTINGS))
    (is (is-search-dir? (file "..") DEFAULT-SETTINGS))
    (is (not (is-search-dir? (file ".git") DEFAULT-SETTINGS)))
    (is (is-search-dir? (file "clojure") DEFAULT-SETTINGS))))

(deftest test-is-search-dir?-with-in-dirpatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "search" :in-dirpatterns)]
    (testing "test-is-search-dir?-with-in-dirpatterns"
      (is (is-search-dir? (file "cljsearch") settings))
      (is (is-search-dir? (file "searcher") settings))
      (is (not (is-search-dir? (file "clojure") settings))))))

(deftest test-is-search-dir?-with-out-dirpatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "clojure" :out-dirpatterns)]
    (testing "test-is-search-dir?-with-out-dirpatterns"
      (is (is-search-dir? (file "cljsearch") settings))
      (is (is-search-dir? (file "searcher") settings))
      (is (not (is-search-dir? (file "clojure") settings))))))

(deftest test-is-search-dir?-with-include-hidden
  (let [settings (assoc DEFAULT-SETTINGS :excludehidden false)]
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

(deftest test-is-search-file?-with-in-filepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "search" :in-filepatterns)]
    (testing "test-is-search-file?-with-in-filepatterns"
      (is (is-search-file? (file "cljsearch.clj") settings))
      (is (is-search-file? (file "searcher.clj") settings))
      (is (not (is-search-file? (file "fileutil.clj") settings))))))

(deftest test-is-search-file?-with-out-filepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "search" :out-filepatterns)]
    (testing "test-is-search-dir?-with-out-filepatterns"
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
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archiveextensions)]
    (testing "test-is-archive-search-file?-with-in-archiveextensions"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "archive.bz2") settings))
      (is (not (is-archive-search-file? (file "archive.gz") settings))))))

(deftest test-is-archive-search-file?-with-out-archiveextensions
  (let [settings (add-extension DEFAULT-SETTINGS "gz" :out-archiveextensions)]
    (testing "test-is-archive-search-file?-with-out-archiveextensions"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "archive.bz2") settings))
      (is (not (is-archive-search-file? (file "archive.gz") settings))))))

(deftest test-is-archive-search-file?-with-in-archivefilepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "arch" :in-archivefilepatterns)]
    (testing "test-is-archive-search-file?-with-in-archivefilepatterns"
      (is (is-archive-search-file? (file "archive.zip") settings))
      (is (is-archive-search-file? (file "arch.bz2") settings))
      (is (not (is-archive-search-file? (file "compressed.gz") settings))))))

(deftest test-is-archive-search-file?-with-out-archivefilepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "compress" :out-archivefilepatterns)]
    (testing "test-is-archive-search-file?-with-out-archivefilepatterns"
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
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archiveextensions)]
    (testing "test-filter-file?-with-archive-search-file-settings"
      (is (not (filter-file? (file "archive.zip") settings)))
      (is (not (filter-file? (file "archive.bz2") settings)))
      (is (not (filter-file? (file "archive.gz") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-searcharchives
  (let [settings (assoc DEFAULT-SETTINGS :searcharchives true)]
    (testing "test-filter-file?-with-searcharchives"
      (is (filter-file? (file "archive.zip") settings))
      (is (filter-file? (file "archive.bz2") settings))
      (is (filter-file? (file "archive.gz") settings))
      (is (filter-file? (file "searcher.clj") settings))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-archivesonly
  (let [settings (set-archivesonly DEFAULT-SETTINGS)]
    (testing "test-filter-file?-with-archivesonly"
      (is (filter-file? (file "archive.zip") settings))
      (is (filter-file? (file "archive.bz2") settings))
      (is (filter-file? (file "archive.gz") settings))
      (is (not (filter-file? (file "searcher.clj") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-includehidden
  (let [settings (assoc DEFAULT-SETTINGS :excludehidden false)]
    (testing "test-filter-file?-with-includehidden"
      (is (filter-file? (file "searcher.clj") settings))
      (is (filter-file? (file ".gitignore") settings))
      (is (not (filter-file? (file "archive.zip") settings))))))

