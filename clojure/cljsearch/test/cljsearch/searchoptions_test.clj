(ns cljsearch.searchoptions-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)])
  (:use [cljsearch.searchoptions :only (settings-from-args)]))

(deftest test-no-args
  (let [[ss errs] (settings-from-args [])]
    (testing "test-no-args"
      (is (not (:archivesonly ss)))
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
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Search" "."])]
    (testing "test-valid-args"
      (is (empty? errs))
      (is (= (count (:in-extensions ss)) 1))
      (is (contains? (:in-extensions ss) "clj"))
      (is (= (count (:searchpatterns ss)) 1))
      (is (= (.pattern (first (:searchpatterns ss))) "Search"))
      (is (= (:startpath ss) ".")))))

(deftest test-invalid-args
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Search" "." "-Q"])]
    (testing "test-invalid-args"
      (is (= (count errs) 1))
      (is (= (first errs) "Invalid option: Q")))))
