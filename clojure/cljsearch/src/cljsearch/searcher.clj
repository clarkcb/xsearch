;;; search.clj: Recursive file search utility

;; by Cary Clark
;; April 10, 2010

(ns cljsearch.searcher
  #^{:author "Cary Clark",
     :doc "Recursive file search utility"}
  (:require [cljfind.fileresult]
            [cljsearch.searchresult]
            [cljsearch.searchsettings])
  (:import (java.io File)
           (java.util.jar JarFile)
           (java.util.zip ZipFile)
           (cljfind.fileresult FileResult)
           (cljsearch.searchresult SearchResult)
           (cljsearch.searchsettings SearchSettings)
           )
  (:use [clojure.java.io :only (file reader)]
        [clojure.string :as str :only (join trim upper-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileresult :only (file-result-path)]
        [cljfind.fileutil :only (get-parent-name path-str)]
        [cljfind.finder :only (find-files print-matching-dirs print-matching-files validate-settings)]
        [cljsearch.searchresult :only
          (->SearchResult get-line-formatter search-result-to-string)]
        [cljsearch.searchsettings]
        ))

(defn validate-path [path]
  (if (not path)
    "Startpath not defined"
    (let [file-path (if path (file path) nil)]
      (if (or (not file-path) (not (.exists file-path)))
        "Startpath not found"
        (if (not (.canRead file-path))
          "Startpath not readable"
          nil)))))

(defn validate-paths [paths]
  (if (or (not paths) (empty? paths))
    ["Startpath not defined"]
    (take 1 (filter #(not (= % nil)) (map validate-path paths)))))

(defn validate-search-settings [^SearchSettings settings]
  (let [find-errs (validate-settings settings)]
    (if (not (empty? find-errs))
      find-errs
      (let [tests [(fn [ss] (if (empty? (:search-patterns ss)) "No search patterns defined" nil))
                   (fn [ss]
                     (if
                       (not
                         (=
                           (try
                             (java.nio.charset.Charset/forName (:text-file-encoding ss))
                             (catch IllegalArgumentException e nil))
                           nil)
                       ) nil (format "Invalid encoding: %s" (:text-file-encoding ss))))
                   (fn [ss] (if (< (:lines-after ss) 0) "Invalid lines-after" nil))
                   (fn [ss] (if (< (:lines-before ss) 0) "Invalid lines-before" nil))
                   (fn [ss] (if (< (:max-line-length ss) 0) "Invalid max-line-length" nil))
                  ]]
        (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))))

(defn search-archive-file [f ^SearchSettings settings]
  (if (:verbose settings)
    (log-msg (format "Searching archive file %s" f))))

(defn search-binary-string-for-pattern
  ([b p ^SearchSettings settings]
    (let [m (re-matcher p b)]
      (if (.find m 0)
        (search-binary-string-for-pattern b m 0 settings)
        [])))
  ([b m i ^SearchSettings settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              result (->SearchResult
                       (.pattern m)
                       nil
                       0
                       (+ startmatchindex 1)
                       (+ endmatchindex 1)
                       ""
                       []
                       [])]
          (if (:first-match settings)
            [result]
            (concat [result] (search-binary-string-for-pattern b m
              endmatchindex settings)))))
      [])))

(defn search-binary-string [b ^SearchSettings settings]
  (if (:debug settings)
    (log-msg "Searching binary string"))
  (apply concat
    (map #(search-binary-string-for-pattern b % settings) (:search-patterns settings))))

(defn search-binary-file [fr ^SearchSettings settings]
  (if (:verbose settings)
    (log-msg (format "Searching binary file %s" (file-result-path fr))))
  (let [contents (slurp (:file fr) :encoding "ISO-8859-1") ; use single-byte enc to avoid corruption
        search-results (search-binary-string contents settings)
        with-file-results (map #(assoc-in % [:file] fr) search-results)]
    with-file-results))

(defn matches-any-pattern? [^String s pp]
  (some #(re-find % s) pp))

(defn any-matches-any-pattern? [ss pp]
  (some #(not (= % nil)) (map #(matches-any-pattern? % pp) ss)))

(defn lines-match? [lines inpatterns outpatterns]
  (and
    (or
      (empty? inpatterns)
      (any-matches-any-pattern? lines inpatterns))
    (or
      (empty? outpatterns)
      (not (any-matches-any-pattern? lines outpatterns)))))

(defn lines-before-match? [lines-before ^SearchSettings settings]
  (lines-match? lines-before (:in-lines-before-patterns settings) (:out-lines-before-patterns settings)))

(defn lines-after-match? [lines-after ^SearchSettings settings]
  (lines-match? lines-after (:in-lines-after-patterns settings) (:out-lines-after-patterns settings)))

(defn get-newline-indices [^String s]
  (map first 
    (filter #(= (second %) \newline)
      (map-indexed vector s))))

(defn get-multiline-lines-before [^String s beforestartindices beforeendindices ^SearchSettings settings]
  (if (> (:lines-before settings) 0)
    (let [lines-before (:lines-before settings)
          startindices (take-last lines-before beforestartindices)
          endindices (take-last lines-before beforeendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn get-multiline-lines-after [^String s afterstartindices afterendindices ^SearchSettings settings]
  (if (> (:lines-after settings) 0)
    (let [lines-after (:lines-after settings)
          startindices (take lines-after afterstartindices)
          endindices (take lines-after afterendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn search-multiline-string-for-pattern
  ([^String s p ^SearchSettings settings]
    (let [m (re-matcher p s)]
      (if (.find m 0)
        (let [newlineindices (get-newline-indices s)
              startlineindices (concat [0] (map inc newlineindices))
              endlineindices (concat newlineindices [(count s)])]
          (search-multiline-string-for-pattern s m 0 startlineindices
            endlineindices settings)))))
  ([^String s m i startlineindices endlineindices ^SearchSettings settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              beforestartindices (filter #(<= % startmatchindex) startlineindices)
              beforeendindices (filter #(< % startmatchindex) endlineindices)
              startlineindex (apply max beforestartindices)
              endlineindex (apply min (filter #(> % startmatchindex) endlineindices))
              line (.substring s startlineindex endlineindex)
              line-num (count beforestartindices)
              lines-before (get-multiline-lines-before s (butlast beforestartindices)
                beforeendindices settings)
              afterstartindices (filter #(> % startmatchindex) startlineindices)
              afterendindices (filter #(> % startmatchindex) endlineindices)
              lines-after (get-multiline-lines-after s afterstartindices
                (rest afterendindices) settings)
              result (->SearchResult
                       (.pattern m)
                       nil 
                       line-num
                       (+ (- startmatchindex startlineindex) 1)
                       (+ (- endmatchindex startlineindex) 1)
                       line
                       lines-before
                       lines-after)]
          (if
            (and
              (or
                (= (:lines-before settings) 0)
                (lines-before-match? lines-before settings))
              (or
                (= (:lines-after settings) 0)
                (lines-after-match? lines-after settings)))
            (if (:first-match settings)
              [result]
              (concat [result] (search-multiline-string-for-pattern s m
                endmatchindex startlineindices endlineindices settings)))
            [])))
      [])))

(defn search-multiline-string [^String s ^SearchSettings settings]
  (apply concat
    (map #(search-multiline-string-for-pattern s % settings) (:search-patterns settings))))

(defn search-text-file-contents [^FileResult fr ^SearchSettings settings]
  (let [contents (slurp (:file fr) :encoding (:text-file-encoding settings))
        search-results (search-multiline-string contents settings)
        with-file-results (map #(assoc-in % [:file] fr) search-results)]
    with-file-results))

(defn search-line-for-pattern
  ([line-num line lines-before lines-after p ^SearchSettings settings]
    (let [m (re-matcher p line)]
      (if
        (and
          (.find m 0)
          (lines-before-match? lines-before settings)
          (lines-after-match? lines-after settings))
        (search-line-for-pattern line-num line lines-before lines-after m 0 [] settings)
        [])))
  ([line-num line lines-before lines-after m i results ^SearchSettings settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              result (->SearchResult
                       (.pattern m)
                       nil 
                       line-num
                       (+ startmatchindex 1)
                       (+ endmatchindex 1)
                       line
                       lines-before
                       lines-after)]
          (search-line-for-pattern line-num line lines-before lines-after m
            endmatchindex (concat results [result]) settings)))
      results)))

(defn search-line [line-num line lines-before lines-after ^SearchSettings settings]
  (apply concat
    (map #(search-line-for-pattern line-num line lines-before lines-after % settings)
      (:search-patterns settings))))

(defn search-lines
  ([lines ^SearchSettings settings]
    (let [line (first lines)
          nextlines (drop (:lines-after settings) (rest lines))
          lines-before []
          lines-after (take (:lines-after settings) (rest lines))]
      (search-lines 1 line nextlines lines-before lines-after [] settings)))
  ([line-num line lines lines-before lines-after results ^SearchSettings settings]
    (if line
      (let [nextresults (search-line line-num line lines-before lines-after settings)
            nextline-num (+ line-num 1)
            nextline (if (empty? lines-after) (first lines) (first lines-after))
            nextlines-before
              (if (> (:lines-before settings) 0)
                (if (= (count lines-before) (:lines-before settings))
                  (concat (rest lines-before) [line])
                  (concat lines-before [line]))
                [])
            nextlines-after
              (if (> (:lines-after settings) 0)
                (concat (rest lines-after) (take 1 lines))
                [])]
          (search-lines nextline-num nextline (rest lines) nextlines-before
            nextlines-after (concat results nextresults) settings))
      (if
        (and
          (> (count results) 0)
          (:first-match settings))
        (take 1 results)
        results))))

(defn search-text-file-lines [^FileResult fr ^SearchSettings settings]
  (with-open [rdr (reader (.toFile (:path fr)) :encoding (:text-file-encoding settings))]
    (let [search-results (search-lines (line-seq rdr) settings)
          with-file-results (map #(assoc-in % [:file] fr) search-results)]
      with-file-results)))

(defn search-text-file [^FileResult fr ^SearchSettings settings]
  (if (:verbose settings)
    (log-msg (format "Searching text file %s" (file-result-path fr))))
  (if (:multi-line-search settings)
    (search-text-file-contents fr settings)
    (search-text-file-lines fr settings)))

(defn search-file [^FileResult fr ^SearchSettings settings]
  (let [file-type (:file-type fr)
        verbose (:verbose settings)
        file-path (file-result-path fr)]
    (cond
      (or
        (= file-type :code)
        (= file-type :text)
        (= file-type :xml)) (search-text-file fr settings)
      (= file-type :binary) (search-binary-file fr settings)
      (= file-type :archive)
        (if (:search-archives settings)
          (search-archive-file fr settings)
          (do
            (if verbose (log-msg (format "Skipping archive file %s" file-path))
            [])))
      :else
        (do
          (if verbose (log-msg (format "Skipping file of unknown type: %s" file-path))
          [])))))

(defn search-files [search-files ^SearchSettings settings]
  (if (:verbose settings)
    (do
      (log-msg (format "\nFiles to be searched (%d):" (count search-files)))
      (doseq [fr search-files] (log-msg (file-result-path fr)))
      (log-msg "")))
  (apply concat (map #(search-file % settings) search-files)))

(defn search [^SearchSettings settings]
  (let [validation-errs (validate-search-settings settings)]
    (if (empty? validation-errs)
      (let [find-settings (to-find-settings settings)
            [file-results find-errs] (find-files find-settings)]
        (if (empty? find-errs)
          [(search-files file-results settings) []]
          [[] find-errs]))
      [[] validation-errs])))

(defn print-search-result [^SearchResult r ^SearchSettings settings]
  (log-msg (search-result-to-string r settings)))

(defn print-search-results [results ^SearchSettings settings]
  (if (> (count results) 0)
    (let [hdr (format "\nSearch results (%d):" (count results))]
      (log-msg hdr)
      (doseq [r results] (print-search-result r settings)))
    (log-msg "\nSearch results: 0")))

(defn get-search-results-matching-files [results]
  (let [file-paths (map #(file-result-path (:file %)) results)
        file-map (zipmap file-paths (map #(:file %) results))]
    (sort-by (fn [f] (file-result-path f)) (vals file-map))))

(defn print-search-results-matching-dirs [results ^SearchSettings settings]
  (let [files (get-search-results-matching-files results)]
    (print-matching-dirs files settings)))

(defn print-search-results-matching-files [results ^SearchSettings settings]
  (let [files (get-search-results-matching-files results)]
    (print-matching-files files settings)))

(defn get-search-results-matching-lines [results ^SearchSettings settings]
  (let [lines (sort-by str/upper-case (map #(str/trim (:line %)) results))]
    (if (:unique-lines settings)
      (distinct lines)
      lines)))

(defn print-search-results-matching-lines [results ^SearchSettings settings]
  (let [format-line (get-line-formatter settings)
        lines (get-search-results-matching-lines results settings)
        hdr (if (:unique-lines settings)
              "\nUnique lines with matches"
              "\nLines with matches")]
    (if (> (count lines) 0)
      (let [hdr1 (format "%s (%d):" hdr (count lines))]
        (log-msg hdr1)
        (doseq [l lines] (log-msg (format-line l))))
      (log-msg (format "%s: 0" hdr)))))
