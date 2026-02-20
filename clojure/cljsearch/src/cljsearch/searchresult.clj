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
              (colorize-string line (.start line-matcher) (.end line-matcher) (:line-color settings))))]
    color-line))

(defn get-line-formatter [^SearchSettings settings]
  (if
    (:colorize settings)
    (fn [^String line]
      (colorize-line line settings))
    (fn [^String line]
      line)))

(defn colorize-match ^String [^String match ^SearchSettings settings]
  (colorize-string match 0 (.length match) (:line-color settings)))

(defn get-match-formatter [^SearchSettings settings]
  (if
    (:colorize settings)
    (fn [^String match]
      (colorize-match match settings))
    (fn [^String match]
      match)))

(defn format-result-match [^SearchResult r ^SearchSettings settings]
  (if (or (= (str/trim (:line r)) "") (= (:max-line-length settings) 0))
    ""
    (let [match-start-idx (dec (:matchstartindex r))
          match-end-idx (dec (:matchendindex r))
          match-length (- match-end-idx match-start-idx)
          gt-max-length (> match-length (:max-line-length settings))
          prefix (if (and gt-max-length (> match-start-idx 2)) "..." "")
          suffix (if gt-max-length "..." "")
          match-str (if gt-max-length
                      (str prefix (subs (:line r) match-start-idx (+ match-start-idx (- (:max-line-length settings) (.length prefix) 3))) suffix)
                      (str prefix (subs (:line r) match-start-idx match-end-idx) suffix))
          color-start-idx (.length prefix)
          color-end-idx (if gt-max-length (- (:max-line-length settings) 3) match-length)]
      (if (:colorize settings)
        (colorize-string match-str color-start-idx color-end-idx (:line-color settings))
        match-str))))

;;; find the start-idx and end-idx that equal max-length without going beyond first or last idx
(defn get-max-length-indices
  ([start-idx end-idx max-idx max-length]
   (get-max-length-indices start-idx end-idx 0 max-idx max-length))
  ([start-idx end-idx min-idx max-idx max-length]
    (cond
      (= (- max-idx min-idx) max-length) [min-idx max-idx]
      (= (- end-idx start-idx) max-length) [start-idx end-idx]
      :else
      (let [next-start-idx (if (> start-idx min-idx) (dec start-idx) start-idx)
            next-end-idx (if (< (- end-idx next-start-idx) max-idx) (inc end-idx) end-idx)]
        (get-max-length-indices next-start-idx next-end-idx min-idx max-idx max-length))
    )))
;    (if (= (- end-idx start-idx) max-length)
;      [start-idx end-idx]
;      (let [next-start-idx (if (> start-idx 0) (dec start-idx) start-idx)
;            next-end-idx (if (< (- end-idx next-start-idx) max-length) (inc end-idx) end-idx)]
;        (get-max-length-indices next-start-idx next-end-idx max-length)))))

(defn get-string-indices [^String s match-start-idx match-end-idx max-length]
  (cond
    (= (str/trim s) "") [0, 0, 0, 0]
    (= max-length 0) [0, 0, 0, 0]
    (< max-length 0) (get-string-indices s match-start-idx match-end-idx (inc (.length s)))
    :else
      (let [s-length (.length s)
            line-start-idx (- s-length (.length (str/triml s)))
            line-end-idx (- (dec s-length) (- s-length (.length (str/trimr s))))
            trimmed-length (- line-end-idx line-start-idx)
            match-length (- match-end-idx match-start-idx)
            match-start-idx' (- match-start-idx line-start-idx)
            match-end-idx' (+ match-start-idx' match-length)
            [lsi lei]
              (if (> trimmed-length max-length)
                (get-max-length-indices match-start-idx' match-end-idx' trimmed-length max-length)
                [line-start-idx (inc line-end-idx)])
            msi (- match-start-idx lsi)
            mei (+ msi match-length)]
        [lsi lei msi mei])))

(defn format-result-line-with-match [^SearchResult r ^SearchSettings settings]
  (if (or (= (str/trim (:line r)) "") (= (:max-line-length settings) 0))
    ""
    (let [line (:line r)
          line-length (.length line)
          match-start-idx (dec (:matchstartindex r))
          match-end-idx (dec (:matchendindex r))
          match-length (- match-end-idx match-start-idx)
          max-line-length (if (< (:max-line-length settings) 0) (inc line-length) (:max-line-length settings))
          max-limit (> max-line-length 0)
          [lsi lei msi mei] (get-string-indices line match-start-idx match-end-idx max-line-length)
          line-length' (- lei lsi)]
      (if (= line-length' 0)
        ""
        (let [prefix (if (and max-limit (> lsi 2)) "..." "")
              suffix (if (and max-limit (< lei (- line-length 2))) "..." "")
              lsi' (+ lsi (.length prefix))
              lei' (- lei (.length suffix))
              truncated (str prefix (subs line lsi' lei') suffix)]
          (if (:colorize settings)
            (colorize-string truncated msi mei (:line-color settings))
            truncated))))))

(defn format-result-line [^SearchResult r ^SearchSettings settings]
  (if (or (= (str/trim (:line r)) "") (= (:max-line-length settings) 0))
    ""
    (let [match-length (- (:matchendindex r) (:matchstartindex r))
          max-length
            (cond
              (= (:max-line-length settings) 0) 0
              (< (:max-line-length settings) 0) (inc match-length)
              :else (:max-line-length settings))]
      (if (> match-length max-length)
        (format-result-match r settings)
        (format-result-line-with-match r settings)))))

(defn get-multi-line-formatter [^SearchSettings settings]
  (let [format-file-result (get-file-result-formatter settings)
        format-line (get-line-formatter settings)]
    (fn [^SearchResult r]
      (let [line-num (:line-num r)
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
                     lines-after-indexed)))))))

(defn get-single-line-formatter [^SearchSettings settings]
  (let [format-file-result (get-file-result-formatter settings)]
    (fn [^SearchResult r]
      (if (> (:line-num r) 0)
        (str
         (format-file-result (:file r)) ": " (:line-num r) ": [" (:matchstartindex r) ":"
         (:matchendindex r) "]: " (format-result-line r settings))
        (str (file-result-path (:file r)) " matches at [" (:matchstartindex r) ":"
             (:matchendindex r) "]")))))

(defn get-search-result-formatter [^SearchSettings settings]
  (if
    (or
     (> (:lines-before settings) 0)
     (> (:lines-after settings) 0))
    (get-multi-line-formatter settings)
    (get-single-line-formatter settings)))
