(ns cljsearch.searchoptions-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileutil :only (to-path)]
        [cljsearch.searchoptions :only (settings-from-args settings-from-json)]))

(deftest test-no-args
  (let [[ss errs] (settings-from-args [])]
    (testing "test-no-args"
      (is (not (:archives-only ss)))
      (is (:colorize ss))
      (is (not (:debug ss)))
      (is (not (:first-match ss)))
      (is (not (:include-hidden ss)))
      (is (= (:lines-after ss) 0))
      (is (= (:lines-before ss) 0))
      (is (not (:list-dirs ss)))
      (is (not (:list-files ss)))
      (is (not (:list-lines ss)))
      (is (= (:max-line-length ss) 150))
      (is (not (:multi-line-search ss)))
      (is (:print-results ss))
      (is (not (:print-usage ss)))
      (is (not (:print-version ss)))
      (is (:recursive ss))
      (is (not (:search-archives ss)))
      (is (= (:startpath ss) nil))
      (is (not (:unique-lines ss)))
      (is (not (:verbose ss))))))

(deftest test-valid-args
  (let [[ss errs] (settings-from-args ["-x" "clj,scala" "-s" "Search" "."])]
    (testing "test-valid-args"
      (is (empty? errs))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "clj"))
      (is (contains? (:in-extensions ss) "scala"))
      (is (= (count (:search-patterns ss)) 1))
      (is (= (.pattern (first (:search-patterns ss))) "Search"))
      (is (= (count (:paths ss)) 1))
      (is (contains? (:paths ss) (to-path "."))))))

(deftest test-missing-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Search" "." "-D"])]
    (testing "test-missing-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Missing arg for option out-dirpattern")))))

(deftest test-invalid-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Search" "." "-Q"])]
    (testing "test-invalid-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Invalid option: Q")))))

;(deftest test-archives-only
;  (let [[ss errs] (settings-from-args ["--archivesonly"])]
;    (testing "test-archives-only"
;      (is (= (:archives-only ss) true))
;      (is (= (:search-archives ss) true)))))

(deftest test-debug
  (let [[ss errs] (settings-from-args ["--debug"])]
    (testing "test-debug"
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true)))))

(deftest test-settings-from-json
  (let [settings-json "{
  \"path\": \"~/src/xsearch\",
  \"in-ext\": [\"js\",\"ts\"],
  \"out-dirpattern\": \"node_module\",
  \"out-filepattern\": [\"temp\"],
  \"searchpattern\": \"Searcher\",
  \"linesbefore\": 2,
  \"linesafter\": 2,
  \"debug\": true,
  \"allmatches\": false,
  \"includehidden\": true
}"
        [ss errs] (settings-from-json settings-json)
        ]
    (testing "test-debug"
      (is (= (count (:paths ss)) 1))
      (is (contains? (:paths ss) (to-path "~/src/xsearch")))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "js"))
      (is (contains? (:in-extensions ss) "ts"))
      (is (= (count (:out-dir-patterns ss)) 1))
      (is (= (count (:out-file-patterns ss)) 1))
      (is (= (count (:search-patterns ss)) 1))
      (is (= (:lines-before ss) 2))
      (is (= (:lines-after ss) 2))
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true))
      (is (= (:first-match ss) true))
      (is (= (:exclude-hidden ss) false)))))
