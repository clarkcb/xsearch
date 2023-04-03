(ns cljsearch.searchoptions-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljsearch.searchoptions :only (settings-from-args settings-from-json)]))

(deftest test-no-args
  (let [[ss errs] (settings-from-args [])]
    (testing "test-no-args"
      (is (not (:archivesonly ss)))
      (is (:colorize ss))
      (is (not (:debug ss)))
      (is (:excludehidden ss))
      (is (not (:firstmatch ss)))
      (is (= (:linesafter ss) 0))
      (is (= (:linesbefore ss) 0))
      (is (not (:listdirs ss)))
      (is (not (:listfiles ss)))
      (is (not (:listlines ss)))
      (is (= (:maxlinelength ss) 150))
      (is (not (:multilinesearch ss)))
      (is (:printresults ss))
      (is (not (:printusage ss)))
      (is (not (:printversion ss)))
      (is (:recursive ss))
      (is (not (:searcharchives ss)))
      (is (= (:startpath ss) nil))
      (is (not (:uniquelines ss)))
      (is (not (:verbose ss))))))

(deftest test-valid-args
  (let [[ss errs] (settings-from-args ["-x" "clj,scala" "-s" "Search" "."])]
    (testing "test-valid-args"
      (is (empty? errs))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "clj"))
      (is (contains? (:in-extensions ss) "scala"))
      (is (= (count (:searchpatterns ss)) 1))
      (is (= (.pattern (first (:searchpatterns ss))) "Search"))
      (is (= (count (:paths ss)) 1))
      (is (contains? (:paths ss) ".")))))

(deftest test-missing-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Search" "." "-D"])]
    (testing "test-missing-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Missing arg for option D")))))

(deftest test-invalid-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Search" "." "-Q"])]
    (testing "test-invalid-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Invalid option: Q")))))

(deftest test-archivesonly
  (let [[ss errs] (settings-from-args ["--archivesonly"])]
    (testing "test-archivesonly"
      (is (= (:archivesonly ss) true))
      (is (= (:searcharchives ss) true)))))

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
      (is (contains? (:paths ss) "~/src/xsearch"))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "js"))
      (is (contains? (:in-extensions ss) "ts"))
      (is (= (count (:out-dirpatterns ss)) 1))
      (is (= (count (:out-filepatterns ss)) 1))
      (is (= (count (:searchpatterns ss)) 1))
      (is (= (:linesbefore ss) 2))
      (is (= (:linesafter ss) 2))
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true))
      (is (= (:firstmatch ss) true))
      (is (= (:excludehidden ss) false)))))
