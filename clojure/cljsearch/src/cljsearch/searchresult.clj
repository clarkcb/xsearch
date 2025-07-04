;;; ############################################################################
;;;
;;; search-result.clj
;;;
;;; Record and functionality for search results
;;;
;;; ############################################################################

(ns cljsearch.searchresult
  #^{:author "Cary Clark",
     :doc "Search results record and functions"}
  (:require [cljsearch.searchsettings])
  (:import (cljsearch.searchsettings SearchSettings))
  (:use [clojure.string :as str :only (trim trimr trim-newline)]
        [cljfind.fileresult :only (colorize-string file-result-path get-file-result-formatter)]
        ))

; record to hold a search-result (file is a FileResult record instance)
(defrecord SearchResult [pattern file line-num matchstartindex matchendindex line
                         lines-before lines-after])

(defn colorize-line ^String [^String line ^SearchSettings settings]
  (let [matching-search-patterns (take 1 (filter #(re-find % line) (:search-patterns settings)))
        line-matcher
          (if (empty? matching-search-patterns)
            nil
            (re-matcher (first matching-search-patterns) line))
        color-line
          (if (nil? line-matcher)
            line
            (do
              (.find line-matcher 0)
              (colorize-string line (.start line-matcher) (.end line-matcher))))]
    color-line))

(defn get-line-formatter [^SearchSettings settings]
  (if
    (:colorize settings)
    (fn [^String line]
      (colorize-line line settings))
    (fn [^String line]
      line)))

(defn multi-line-to-string ^String [^SearchResult r ^SearchSettings settings]
  (let [format-file-result (get-file-result-formatter settings)
        format-line (get-line-formatter settings)
        line-num (:line-num r)
        lines-before (map #(str/trim-newline %) (:lines-before r))
        line (str/trim-newline (:line r))
        lines-after (map #(str/trim-newline %) (:lines-after r))
        maxline-num (+ line-num (count lines-after))
        line-numpadding (count (str maxline-num))
        lines-format (str "%1$s %2$" line-numpadding "d | %3$s\n")
        lines-before-indexed (map-indexed vector lines-before)
        lines-after-indexed (map-indexed vector lines-after)]
    (str
      (apply str (take 80 (repeat "="))) "\n"
      (format-file-result (:file r)) ": " (:line-num r) ": [" (:matchstartindex r) ":" (:matchendindex r) "]\n"
      (apply str (take 80 (repeat "-"))) "\n"
      (apply str
        (map #(format lines-format " " (+ (- line-num (count lines-before)) (first %))
          (second %)) lines-before-indexed))
      (format lines-format ">" line-num (format-line line))
      (apply str
        (map #(format lines-format " " (+ line-num (first %) 1) (second %))
          lines-after-indexed)))))

(defn rec-get-indices [^SearchSettings settings linestartindex lineendindex]
  (if (< (- lineendindex linestartindex) (:max-line-length settings))
    (let [lsi (if (> linestartindex 0) (- linestartindex 1) linestartindex)
          lei (if (< (- lineendindex lsi) (:max-line-length settings)) (+ lineendindex 1) lineendindex)]
      (rec-get-indices settings lsi lei))
    { :linestartindex linestartindex
      :lineendindex lineendindex }))

(defn format-matching-line [^SearchResult r ^SearchSettings settings]
  (let [trimmed (str/trim (:line r))
        leading-whitespace-count (- (count (str/trimr (:line r))) (count trimmed))
        match-length (- (:matchendindex r) (:matchstartindex r))
        adj-match-start-index (- (:matchstartindex r) 1 leading-whitespace-count)
        adj-match-end-index (+ adj-match-start-index match-length)
        formatted (colorize-string trimmed adj-match-start-index adj-match-end-index)]
    formatted))

(defn single-line-to-string ^String [^SearchResult r ^SearchSettings settings]
  (let [format-file-result (get-file-result-formatter settings)]
    (if (> (:line-num r) 0)
      (str
        (format-file-result (:file r)) ": " (:line-num r) ": [" (:matchstartindex r) ":"
        (:matchendindex r) "]: " (format-matching-line r settings))
      (str (file-result-path (:file r)) " matches at [" (:matchstartindex r) ":"
        (:matchendindex r) "]"))))

(defn search-result-to-string ^String [^SearchResult r ^SearchSettings settings]
  (if
    (or
      (not (empty? (:lines-before r)))
      (not (empty? (:lines-after r))))
    (multi-line-to-string r settings)
    (single-line-to-string r settings)))
