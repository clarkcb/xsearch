(ns cljsearch.fileutil-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as str :only (join)])
  (:use [cljsearch.fileutil]))

(deftest test-get-ext
  (testing "test-get-ext"
    (is (= (get-ext "filename.txt") "txt"))
    (is (= (get-ext "filename.") ""))
    (is (= (get-ext "filename") ""))
    (is (= (get-ext ".filename.txt") "txt"))
    (is (= (get-ext ".filename.") ""))
    (is (= (get-ext ".filename") ""))))

(deftest test-expand-path
  (let [home (System/getProperty "user.home")]
    (testing "test-expand-path"
      (is (= (expand-path "filename.txt") "filename.txt"))
      (is (= (expand-path "/filename.txt") "/filename.txt"))
      (is (= (expand-path "~/filename.txt") (str/join "/" [home "filename.txt"]))))))

(deftest test-has-ext
  (let [home (System/getProperty "user.home")]
    (testing "test-has-ext"
      (is (not (has-ext? "filename.txt" "xyz")))
      (is (has-ext? "filename.txt" "txt"))
      (is (has-ext? "filename." ""))
      (is (has-ext? "filename" ""))
      (is (has-ext? ".filename.txt" "txt"))
      (is (has-ext? ".filename." ""))
      (is (has-ext? ".filename" "")))))

(deftest test-hidden
  (let [home (System/getProperty "user.home")]
    (testing "test-hidden"
      (is (not (hidden? "filename.txt")))
      (is (not (hidden? ".")))
      (is (not (hidden? "..")))
      (is (hidden? ".filename.txt")))))
