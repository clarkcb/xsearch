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
  (:use [clojure.string :as str :only (trim trimr trim-newline)]
        [cljsearch.color :only (RESET GREEN)]
        [cljfind.fileresult :only (file-result-path)]
        ))

; record to hold a search-result (file is a SearchFile record instance)
(defrecord SearchResult [pattern file line-num matchstartindex matchendindex line
                         lines-before lines-after])

(defn multi-line-to-string [r settings]
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
      (file-result-path (:file r)) ": " (:line-num r) ": [" (:matchstartindex r) ":" (:matchendindex r) "]\n"
      (apply str (take 80 (repeat "-"))) "\n"
      (apply str
        (map #(format lines-format " " (+ (- line-num (count lines-before)) (first %))
          (second %)) lines-before-indexed))
      (format lines-format ">" line-num line)
      (apply str
        (map #(format lines-format " " (+ line-num (first %) 1) (second %))
          lines-after-indexed)))))

(defn rec-get-indices [settings linestartindex lineendindex]
  (if (< (- lineendindex linestartindex) (:max-line-length settings))
    (let [lsi (if (> linestartindex 0) (- linestartindex 1) linestartindex)
          lei (if (< (- lineendindex lsi) (:max-line-length settings)) (+ lineendindex 1) lineendindex)]
      (rec-get-indices settings lsi lei))
    { :linestartindex linestartindex
      :lineendindex lineendindex }))

(defn format-matching-line [r settings]
  (let [trimmed (str/trim (:line r))
        trimmed-length (count trimmed)
        leading-whitespace-count (- (count (str/trimr (:line r))) (count trimmed))
        max-line-end-index (- trimmed-length 1)
        match-length (- (:matchendindex r) (:matchstartindex r))
        adj-match-start-index (- (:matchstartindex r) 1 leading-whitespace-count)
        adj-match-end-index (+ adj-match-start-index match-length)
        indices (if (> trimmed-length (:max-line-length settings))
                  (rec-get-indices settings adj-match-start-index adj-match-end-index)
                  {:linestartindex 0
                   :lineendindex   max-line-end-index})
        line-start-index (if (> (:linestartindex indices) 2) (+ (:linestartindex indices) 3) (:linestartindex indices))
        before (if (> (:linestartindex indices) 2) "..." "")
        line-end-index (if (< (:lineendindex indices) (- max-line-end-index 3)) (- (:lineendindex indices) 3) (+ (:lineendindex indices) 1))
        after (if (< (:lineendindex indices) (- max-line-end-index 3)) "..." "")
        color-start (if (:colorize settings) GREEN "")
        color-stop (if (:colorize settings) RESET "")
        pre-match (str before (subs trimmed line-start-index adj-match-start-index))
        match (str color-start (subs trimmed adj-match-start-index adj-match-end-index) color-stop)
        post-match (if (< adj-match-end-index line-end-index) (str (subs trimmed adj-match-end-index line-end-index) after) "")
        formatted (str pre-match match post-match)]
    formatted))

(defn single-line-to-string [r settings]
  (if (> (:line-num r) 0)
    (str
      (file-result-path (:file r)) ": " (:line-num r) ": [" (:matchstartindex r) ":"
      (:matchendindex r) "]: " (format-matching-line r settings))
    (str (file-result-path (:file r)) " matches at [" (:matchstartindex r) ":"
      (:matchendindex r) "]")))

(defn search-result-to-string [r settings]
  (if
    (or
      (not (empty? (:lines-before r)))
      (not (empty? (:lines-after r))))
    (multi-line-to-string r settings)
    (single-line-to-string r settings)))
