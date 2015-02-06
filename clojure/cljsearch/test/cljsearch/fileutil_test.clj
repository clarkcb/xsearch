(ns cljsearch.fileutil-test
  (:use [clojure.java.io :only (file)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)])
  (:use [cljsearch.fileutil :only
    (expand-path get-ext has-ext? hidden? is-dot-dir? split-path)]))

(deftest test-expand-path
  (let [home (System/getProperty "user.home")]
    (testing "test-expand-path"
      (is (= (expand-path "filename.txt") "filename.txt"))
      (is (= (expand-path "/filename.txt") "/filename.txt"))
      (is (= (expand-path "~/filename.txt")
        (str/join "/" [home "filename.txt"]))))))

(deftest test-get-ext
  (testing "test-get-ext"
    (is (= (get-ext "filename.txt") "txt"))
    (is (= (get-ext "filename.") ""))
    (is (= (get-ext "filename") ""))
    (is (= (get-ext ".filename.txt") "txt"))
    (is (= (get-ext ".filename.") ""))
    (is (= (get-ext ".filename") ""))))

(deftest test-has-ext
  (testing "test-has-ext"
    (is (not (has-ext? "filename.txt" "xyz")))
    (is (has-ext? "filename.txt" "txt"))
    (is (has-ext? "filename." ""))
    (is (has-ext? "filename" ""))
    (is (has-ext? ".filename.txt" "txt"))
    (is (has-ext? ".filename." ""))
    (is (has-ext? ".filename" ""))))

(deftest test-hidden?
  (testing "test-hidden?"
    (is (not (hidden? "filename.txt")))
    (is (not (hidden? ".")))
    (is (not (hidden? "..")))
    (is (hidden? ".filename.txt"))))

(deftest test-is-dot-dir?
  (testing "test-is-dot-dir?"
    (is (is-dot-dir? "."))
    (is (is-dot-dir? ".."))))

(deftest test-split-path
  (testing "test-split-path"
    (let [elems (split-path (file "~/path/to/nowhere"))]
      (is (= (first elems) "~"))
      (is (= (second elems) "path")))))
