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
  (:use [clojure.string :as str :only (trim trimr trim-newline)]
        [cljsearch.color :only (RESET GREEN)]))

; record to hold a search-result
(defrecord SearchResult [pattern file linenum matchstartindex matchendindex line
                         linesbefore linesafter])

(defn multiline-to-string [r settings]
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
      (:file r) ": " (:linenum r) ": [" (:matchstartindex r) ":" (:matchendindex r) "]\n"
      (apply str (take 80 (repeat "-"))) "\n"
      (apply str
        (map #(format lineformat " " (+ (- linenum (count linesbefore)) (first %))
          (second %)) linesbefore-indexed))
      (format lineformat ">" linenum line)
      (apply str
        (map #(format lineformat " " (+ linenum (first %) 1) (second %))
          linesafter-indexed)))))

(defn rec-get-indices [settings linestartindex lineendindex]
  (if (< (- lineendindex linestartindex) (:maxlinelength settings))
    (let [lsi (if (> linestartindex 0) (- linestartindex 1) linestartindex)
          lei (if (< (- lineendindex lsi) (:maxlinelength settings)) (+ lineendindex 1) lineendindex)]
      (rec-get-indices settings lsi lei))
    { :linestartindex linestartindex
      :lineendindex lineendindex }))

(defn format-matching-line [r settings]
  (let [trimmed (str/trim (:line r))
        trimmedlength (count trimmed)
        leading-whitespace-count (- (count (str/trimr (:line r))) (count trimmed))
        maxlineendindex (- trimmedlength 1)
        matchlength (- (:matchendindex r) (:matchstartindex r))
        adjmatchstartindex (- (:matchstartindex r) 1 leading-whitespace-count)
        adjmatchendindex (+ adjmatchstartindex matchlength)
        indices (if (> trimmedlength (:maxlinelength settings))
                  (rec-get-indices settings adjmatchstartindex adjmatchendindex)
                  { :linestartindex 0
                    :lineendindex maxlineendindex })
        linestartindex (if (> (:linestartindex indices) 2) (+ (:linestartindex indices) 3) (:linestartindex indices))
        before (if (> (:linestartindex indices) 2) "..." "")
        lineendindex (if (< (:lineendindex indices) (- maxlineendindex 3)) (- (:lineendindex indices) 3) (+ (:lineendindex indices) 1))
        after (if (< (:lineendindex indices) (- maxlineendindex 3)) "..." "")
        colorstart (if (:colorize settings) GREEN "")
        colorstop (if (:colorize settings) RESET "")
        pre-match (str before (subs trimmed linestartindex adjmatchstartindex))
        match (str colorstart (subs trimmed adjmatchstartindex adjmatchendindex) colorstop)
        post-match (if (< adjmatchendindex lineendindex) (str (subs trimmed adjmatchendindex lineendindex) after) "")
        formatted (str pre-match match post-match)]
    formatted))

(defn singleline-to-string [r settings]
  (if (> (:linenum r) 0)
    (str
      (:file r) ": " (:linenum r) ": [" (:matchstartindex r) ":"
      (:matchendindex r) "]: " (format-matching-line r settings))
    (str (:file r) " matches at [" (:matchstartindex r) ":"
      (:matchendindex r) "]")))

(defn search-result-to-string [r settings]
  (if
    (or
      (not (empty? (:linesbefore r)))
      (not (empty? (:linesafter r))))
    (multiline-to-string r settings)
    (singleline-to-string r settings)))
