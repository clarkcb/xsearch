(ns cljsearch.file-types-test
  (:require [clojure.test :refer :all])
  (:use [cljsearch.filetypes :only (get-file-type)]))

(deftest test-get-file-type-archive-file
  (let [f "archive.zip"]
    (testing "test-get-file-type-archive-file"
      (is (= (get-file-type f) :archive)))))

(deftest test-get-file-type-binary-file
  (let [f "binary.exe"]
    (testing "test-get-file-type-binary-file"
      (is (= (get-file-type f) :binary)))))

(deftest test-get-file-type-text-file
  (let [f "text.txt"]
    (testing "test-get-file-type-text-file"
      (is (= (get-file-type f) :text)))))

(deftest test-get-file-type-code-file
  (let [f "code.clj"]
    (testing "test-get-file-type-code-file"
      (is (= (get-file-type f) :code)))))

(deftest test-get-file-type-xml-file
  (let [f "markup.xml"]
    (testing "test-get-file-type-xml-file"
      (is (= (get-file-type f) :xml)))))

(deftest test-get-file-type-unknown-file
  (let [f "unknown.xyz"]
    (testing "test-get-file-type-unknown-file"
      (is (= (get-file-type f) :unknown)))))
