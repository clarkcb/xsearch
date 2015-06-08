;;; ############################################################################
;;;
;;; searchresult.clj
;;;
;;; Record and functionality for search results
;;;
;;; ############################################################################

(ns cljsearch.searchresult
  #^{:author "Cary Clark",
     :doc "Search results record and functions"}
  (:use [clojure.string :as str :only (trim trim-newline)]))

; record to hold a search-result
(defrecord SearchResult [pattern file linenum startmatchindex endmatchindex line
                         linesbefore linesafter])

(defn multiline-to-string [r]
  (let [linenum (:linenum r)
        linesbefore (map #(str/trim-newline %) (:linesbefore r))
        line (str/trim-newline (:line r))
        linesafter (map #(str/trim-newline %) (:linesafter r))
        maxlinenum (+ linenum (count linesafter))
        linenumpadding (count (str maxlinenum))
        lineformat (str "%1$s %2$" linenumpadding "d | %3$s\n")
        linesbefore-indexed (map-indexed vector linesbefore)
        linesafter-indexed (map-indexed vector linesafter)]
    (str
      (apply str (take 80 (repeat "="))) "\n"
      (:file r) ": " (:linenum r) ": [" (:startmatchindex r) ":" (:endmatchindex r) "]\n"
      (apply str (take 80 (repeat "-"))) "\n"
      (apply str
        (map #(format lineformat " " (+ (- linenum (count linesbefore)) (first %))
          (second %)) linesbefore-indexed))
      (format lineformat ">" linenum line)
      (apply str
        (map #(format lineformat " " (+ linenum (first %) 1) (second %))
          linesafter-indexed)))))

(defn singleline-to-string [r]
  (if (> (:linenum r) 0)
    (str
      (:file r) ": " (:linenum r) ": [" (:startmatchindex r) ":"
      (:endmatchindex r) "]: " (str/trim (:line r)))
    (str (:file r) " matches at [" (:startmatchindex r) ":"
      (:endmatchindex r) "]")))

(defn search-result-to-string [r]
  (if
    (or
      (not (empty? (:linesbefore r)))
      (not (empty? (:linesafter r))))
    (multiline-to-string r)
    (singleline-to-string r)))
