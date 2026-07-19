;;; search.clj: Recursive file search utility

;; by Cary Clark
;; April 10, 2010

(ns cljsearch.searcher
  #^{:author "Cary Clark",
     :doc "Recursive file search utility"}
  (:require [cljfind.fileresult]
            [cljfind.finder]
            [cljsearch.searchresult]
            [cljsearch.searchsettings])
  (:import (java.io File)
           (java.util.jar JarFile)
           (java.util.zip ZipFile)
           (cljfind.fileresult FileResult)
           (cljfind.finder Finder)
           (cljsearch.searchresult SearchResult)
           (cljsearch.searchsettings SearchSettings)
           )
  (:use [clojure.java.io :only (file reader)]
        [clojure.string :as str :only (join trim upper-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileresult :only (file-result-path)]
        [cljfind.fileutil :only (get-parent-name path-str)]
        [cljfind.finder]
        [cljsearch.searchresult :only
          (->SearchResult get-line-formatter get-match-formatter get-search-result-formatter)]
        [cljsearch.searchsettings]
        ))

(defn lines-match? [lines inpatterns outpatterns]
  (and
   (empty-or-any-matches-any-pattern? lines inpatterns)
   (empty-or-not-any-matches-any-pattern? lines outpatterns)))

(defn get-newline-indices [^String s]
  (map first
       (filter #(= (second %) \newline)
               (map-indexed vector s))))

(defn lines-from-lines [start-line line-count lines]
  (vec (take line-count (drop (- start-line 1) lines))))

(defn lines-before-from-lines [current-line line-count lines]
  (let [sl (if (>= line-count current-line) (- line-count current-line) (- current-line line-count))
        lc (if (>= line-count current-line) (- line-count current-line) line-count)]
    (lines-from-lines sl lc lines)))

(defn get-search-results-matching-files [results]
  (let [file-paths (map #(file-result-path (:file %)) results)
        file-map (zipmap file-paths (map #(:file %) results))]
    (sort-by (fn [f] (file-result-path f)) (vals file-map))))


(defprotocol SearcherProtocol
  (validate-search-settings [this])
  (search-archive-file [this f])
  (search-binary-string-for-pattern [this b p]
                                    [this b m i])
  (search-binary-string [this b])
  (search-binary-file [this fr])
  (lines-before-match? [this lines-before])
  (lines-after-match? [this lines-after])
  (get-multiline-lines-before [this s beforestartindices beforeendindices])
  (get-multiline-lines-after [this s afterstartindices afterendindices])
  (search-multiline-string-for-pattern [this s p]
                                       [this s m i startlineindices endlineindices])
  (search-multiline-string [this s])
  (search-text-file-contents [this fr])
  (first-matches-met [this results])
  (search-line-with-lines-for-pattern [this line-num line lines p]
                                      [this line-num line lines-before lines-after m i results])
  (search-line-with-lines [this line-num line lines])
  (search-lines [this lines])
  (search-text-file-lines [this fr])
  (search-text-file [this fr])
  (search-file [this fr])
  (search-files [this search-files])
  (search [this])
  (print-search-results [this results])
;  (get-search-results-matching-files [this results])
  (print-search-results-matching-dirs [this results])
  (print-search-results-matching-files [this results])
  (get-search-results-matching-lines [this results])
  (print-search-results-matching-lines [this results])
  (get-search-results-matches [this results])
  (print-search-results-matches [this results]))

(defrecord Searcher [^SearchSettings settings ^Finder finder]
  SearcherProtocol
  (validate-search-settings [this]
    (let [settings (:settings this)
          finder (:finder this)
          find-errs (validate-settings finder)]
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
                     ]]
          (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))))

  (search-archive-file [this f]
    (let [settings (:settings this)]
      (if (:verbose settings)
        (log-msg (format "Searching archive file %s" f)))))

  (search-binary-string-for-pattern [this b p]
    (let [settings (:settings this)
          m (re-matcher p b)]
      (if (.find m 0)
        (search-binary-string-for-pattern this b m 0)
        [])))

  (search-binary-string-for-pattern [this b m i]
    (let [settings (:settings this)]
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
              (concat [result] (search-binary-string-for-pattern this b m
                endmatchindex)))))
        [])))

  (search-binary-string [this b]
    (let [settings (:settings this)]
      (if (:debug settings)
        (log-msg "Searching binary string"))
      (apply concat
             (map #(search-binary-string-for-pattern this b %) (:search-patterns settings)))))

  (search-binary-file [this fr]
    (let [settings (:settings this)
          file-path (:path fr)]
      (if (:verbose settings)
        (log-msg (format "Searching binary file %s" (file-result-path fr))))
      (let [contents (slurp file-path :encoding "ISO-8859-1") ; use single-byte enc to avoid corruption
            search-results (search-binary-string this contents)
            with-file-results (map #(assoc-in % [:file] fr) search-results)]
      with-file-results)))

  (lines-before-match? [this lines-before]
    (let [settings (:settings this)]
      (lines-match? lines-before (:in-lines-before-patterns settings) (:out-lines-before-patterns settings))))

  (lines-after-match? [this lines-after]
    (let [settings (:settings this)]
      (lines-match? lines-after (:in-lines-after-patterns settings) (:out-lines-after-patterns settings))))

  (get-multiline-lines-before [this s beforestartindices beforeendindices]
    (let [settings (:settings this)]
      (if (> (:lines-before settings) 0)
        (let [lines-before (:lines-before settings)
              startindices (take-last lines-before beforestartindices)
              endindices (take-last lines-before beforeendindices)]
          (if (and startindices endindices)
          (map #(.substring s (first %) (second %)) (map vector startindices endindices))
          []))
      [])))

  (get-multiline-lines-after [this s afterstartindices afterendindices]
    (let [settings (:settings this)]
      (if (> (:lines-after settings) 0)
        (let [lines-after (:lines-after settings)
              startindices (take lines-after afterstartindices)
              endindices (take lines-after afterendindices)]
          (if (and startindices endindices)
          (map #(.substring s (first %) (second %)) (map vector startindices endindices))
          []))
      [])))

  (search-multiline-string-for-pattern [this s p]
    (let [settings (:settings this)
          m (re-matcher p s)]
      (if (.find m 0)
        (let [newlineindices (get-newline-indices s)
              startlineindices (concat [0] (map inc newlineindices))
              endlineindices (concat newlineindices [(count s)])]
          (search-multiline-string-for-pattern this s m 0 startlineindices
            endlineindices))
        [])))

  (search-multiline-string-for-pattern [this s m i startlineindices endlineindices]
    (let [settings (:settings this)]
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
              lines-before (get-multiline-lines-before this s (butlast beforestartindices)
                                                       beforeendindices)
              afterstartindices (filter #(> % startmatchindex) startlineindices)
              afterendindices (filter #(> % startmatchindex) endlineindices)
              lines-after (get-multiline-lines-after this s afterstartindices
                                                     (rest afterendindices))
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
              (lines-before-match? this lines-before))
             (or
              (= (:lines-after settings) 0)
              (lines-after-match? this lines-after)))
            (if (:first-match settings)
              [result]
              (concat [result] (search-multiline-string-for-pattern this s m
                                endmatchindex startlineindices endlineindices)))
            [])))
      [])
    ))

  (search-multiline-string [this s]
    (let [settings (:settings this)]
      (apply concat
             (map #(search-multiline-string-for-pattern this s %) (:search-patterns settings)))))

  (search-text-file-contents [this fr]
    (let [settings (:settings this)
          file-path (:path fr)
          contents (slurp file-path :encoding (:text-file-encoding settings))
          search-results (search-multiline-string this contents)
          with-file-results (map #(assoc-in % [:file] fr) search-results)]
      with-file-results))

  (first-matches-met [this results]
    (let [settings (:settings this)]
      (every? #(> % 0) (map (fn [p] (count (filter #(= p (:pattern %)) results))) (:search-patterns settings)))))

  (search-line-with-lines-for-pattern [this line-num line lines p]
     (let [settings (:settings this)
           m (re-matcher p line)]
       (if (.find m 0)
         (let [lines-before (if (need-lines-before settings) (lines-before-from-lines line-num (:lines-before settings) lines) [])
               lines-after (if (need-lines-after settings) (lines-from-lines (inc line-num) (:lines-after settings) lines) [])]
           (if
             (and
              (lines-before-match? this lines-before)
              (lines-after-match? this lines-after))
             (search-line-with-lines-for-pattern this line-num line lines-before lines-after m 0 [])
             []))
         [])))

  (search-line-with-lines-for-pattern [this line-num line lines-before lines-after m i results]
     (if (.find m i)
       (do
         (let [settings (:settings this)
               startmatchindex (.start m)
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
           (if (:first-match settings)
             [result]
             (search-line-with-lines-for-pattern this line-num line lines-before lines-after m
                                                 endmatchindex (concat results [result])))))
       results))

  (search-line-with-lines [this line-num line lines]
    (let [settings (:settings this)]
      (apply concat
             (map #(search-line-with-lines-for-pattern this line-num line lines %)
                  (:search-patterns settings)))))

  (search-lines [this lines]
    (let [settings (:settings this)]
      (loop [line-num 1
             line (first lines)
             results []]
        (if
          (or (nil? line) (and (:first-match settings) (first-matches-met this results)))
        results
        (recur
          (inc line-num)
          (first (drop line-num lines))
          (concat results (search-line-with-lines this line-num line lines)))))))

  (search-text-file-lines [this fr]
    (with-open [rdr (reader (.toFile (:path fr)) :encoding (:text-file-encoding (:settings this)))]
      (let [search-results (search-lines this (line-seq rdr))
            with-file-results (map #(assoc-in % [:file] fr) search-results)]
        with-file-results)))

  (search-text-file [this fr]
    (if (:verbose (:settings this))
      (log-msg (format "Searching text file %s" (file-result-path fr))))
    (if (:multi-line-search (:settings this))
      ;; Temporarily disabling contents search
      ;    (search-text-file-contents fr settings)
      (search-text-file-lines this fr)
      (search-text-file-lines this fr)))

  (search-file [this fr]
    (let [settings (:settings this)
          file-type (:file-type fr)
          verbose (:verbose settings)
          file-path (file-result-path fr)]
      (cond
        (or
         (= file-type :code)
         (= file-type :text)
         (= file-type :xml)) (search-text-file this fr)
        (= file-type :binary) (search-binary-file this fr)
        (= file-type :archive)
        (if (:search-archives settings)
          (search-archive-file this fr)
          (do
            (if verbose (log-msg (format "Skipping archive file %s" file-path))
              [])))
        :else
        (do
          (if verbose (log-msg (format "Skipping file of unknown type: %s" file-path))
            [])))))

  (search-files [this search-files]
    (let [settings (:settings this)]
      (if (:verbose settings)
        (do
          (log-msg (format "\nFiles to be searched (%d):" (count search-files)))
          (doseq [fr search-files] (log-msg (file-result-path fr)))
          (log-msg "")))
      (apply concat (map #(search-file this %) search-files))))

  (search [this]
    (let [settings (:settings this)
          finder (:finder this)
          validation-errs (validate-search-settings this)]
      (if (empty? validation-errs)
        (let [[file-results find-errs] (find-files finder)]
          (if (empty? find-errs)
            [(search-files this file-results) []]
            [[] find-errs]))
        [[] validation-errs])))

  (print-search-results [this results]
    (let [settings (:settings this)]
      (if (empty? results)
        (log-msg "\nSearch results: 0")
        (let [format-search-result (get-search-result-formatter settings)]
          (log-msg (format "\nSearch results (%d):" (count results)))
          (doseq [r results] (log-msg (format-search-result r)))))))

  (print-search-results-matching-dirs [this results]
    (let [finder (:finder this)
          files (get-search-results-matching-files results)]
      (print-matching-dirs finder files)))

  (print-search-results-matching-files [this results]
    (let [finder (:finder this)
          files (get-search-results-matching-files results)]
      (print-matching-files finder files)))

  (get-search-results-matching-lines [this results]
    (let [settings (:settings this)
          lines (map #(str/trim (:line %)) (filter #(> (:line-num %) 0) results))]
      (cond
        (and (:unique-lines settings) (:sort-case-insensitive settings)) (sort-by str/upper-case (distinct lines))
        (:unique-lines settings) (sort (distinct lines))
        (:sort-case-insensitive settings) (sort-by str/upper-case lines)
        :else (sort lines))))

  (print-search-results-matching-lines [this results]
    (let [settings (:settings this)
          format-line (get-line-formatter settings)
          lines (get-search-results-matching-lines this results)
          hdr (if (:unique-lines settings)
                "\nUnique matching lines"
                "\nMatching lines")]
      (if (> (count lines) 0)
        (let [hdr1 (format "%s (%d):" hdr (count lines))]
          (log-msg hdr1)
          (doseq [l lines] (log-msg (format-line l))))
        (log-msg (format "%s: 0" hdr)))))

  (get-search-results-matches [this results]
    (let [settings (:settings this)
          matches (map #(subs (:line %) (dec (:matchstartindex %)) (dec (:matchendindex %))) (filter #(> (:line-num %) 0) results))]
      (cond
        (and (:unique-lines settings) (:sort-case-insensitive settings)) (sort-by str/upper-case (distinct matches))
        (:unique-lines settings) (sort (distinct matches))
        (:sort-case-insensitive settings) (sort-by str/upper-case matches)
        :else (sort matches))))

  (print-search-results-matches [this results]
    (let [settings (:settings this)
          format-match (get-match-formatter settings)
          matches (get-search-results-matches this results)
          hdr (if (:unique-lines settings)
                "\nUnique matches"
                "\nMatches")]
      (if (> (count matches) 0)
        (let [hdr1 (format "%s (%d):" hdr (count matches))]
          (log-msg hdr1)
          (doseq [m matches] (log-msg (format-match m))))
        (log-msg (format "%s: 0" hdr)))))

  )

(defn create-searcher [^SearchSettings settings]
  (let [find-settings (to-find-settings settings)
        finder (->Finder find-settings)]
    (->Searcher settings finder)))
