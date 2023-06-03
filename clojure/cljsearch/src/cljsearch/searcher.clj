;;; search.clj: Recursive file search utility

;; by Cary Clark
;; April 10, 2010

(ns cljsearch.searcher
  #^{:author "Cary Clark",
     :doc "Recursive file search utility"}
  (:import (java.io File)
           (java.util.jar JarFile)
           (java.util.zip ZipFile))
  (:use [clojure.java.io :only (file reader)]
        [clojure.string :as str :only (join trim upper-case)]
        [cljsearch.common :only (log-msg)]
        [cljsearch.filetypes :only (archive-file? get-file-type)]
        [cljsearch.fileutil :only
          (get-ext get-files-in-directory get-name hidden-dir? hidden-file?
            is-dot-dir?)]
        [cljsearch.searchfile :only (new-search-file search-file-path)]
        [cljsearch.searchresult :only
          (->SearchResult search-result-to-string)]))

(defn is-search-dir? [d settings]
  (or
    (nil? d)
    (is-dot-dir? (get-name d))
    (and
      (or
        (not (:exclude-hidden settings))
        (not (hidden-dir? d)))
      (or
       (empty? (:in-dir-patterns settings))
        (some #(re-find % (.getPath d)) (:in-dir-patterns settings)))
      (or
       (empty? (:out-dir-patterns settings))
        (not-any? #(re-find % (.getPath d)) (:out-dir-patterns settings))))))

(defn print-search-result [r settings]
  (log-msg (search-result-to-string r settings)))

(defn print-search-results [results settings]
  (log-msg (format "\nSearch results (%d):" (count results)))
  (doseq [r results] (print-search-result r settings)))

(defn get-matching-dirs [results]
  (sort (distinct (map #(.getParent (:file %)) (map #(:file %) results)))))

(defn print-matching-dirs [results]
  (let [dirs (get-matching-dirs results)]
    (log-msg (format "\nDirectories with matches (%d):" (count dirs)))
    (doseq [d dirs] (log-msg d))))

(defn get-matching-files [results]
  (sort (distinct (map #(.getPath (:file %)) (map #(:file %) results)))))

(defn print-matching-files [results]
  (let [files (get-matching-files results)]
    (log-msg (format "\nFiles with matches (%d):" (count files)))
    (doseq [f files] (log-msg f))))

(defn get-matching-lines [results settings]
  (let [lines (sort-by str/upper-case (map #(str/trim (:line %)) results))]
    (if (:unique-lines settings)
      (distinct lines)
      lines)))

(defn print-matching-lines [results settings]
  (let [lines (get-matching-lines results settings)]
    (log-msg
      (if (:unique-lines settings)
        (format "\nUnique lines with matches (%d):" (count lines))
        (format "\nLines with matches (%d):" (count lines))))
    (doseq [l lines] (log-msg l))))

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

(defn validate-settings [settings]
  (let [path-errs (take 1 (filter #(not (= % nil)) (validate-paths (:paths settings))))]
    (if (not (empty? path-errs))
      path-errs
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

(defn is-archive-search-file? [f settings]
  (and
    (or
     (empty? (:in-archive-extensions settings))
     (some #(= % (get-ext f)) (:in-archive-extensions settings)))
    (or
     (empty? (:out-archive-extensions settings))
     (not-any? #(= % (get-ext f)) (:out-archive-extensions settings)))
    (or
     (empty? (:in-archive-file-patterns settings))
     (some #(re-find % (.getName f)) (:in-archive-file-patterns settings)))
    (or
     (empty? (:out-archive-file-patterns settings))
     (not-any? #(re-find % (.getName f)) (:out-archive-file-patterns settings)))))

(defn is-search-file? [f settings]
  (and
    (is-search-dir? (.getParentFile f) settings)
    (or
     (empty? (:in-extensions settings))
     (some #(= % (get-ext f)) (:in-extensions settings)))
    (or
     (empty? (:out-extensions settings))
     (not-any? #(= % (get-ext f)) (:out-extensions settings)))
    (or
     (empty? (:in-file-patterns settings))
     (some #(re-find % (.getName f)) (:in-file-patterns settings)))
    (or
     (empty? (:out-file-patterns settings))
     (not-any? #(re-find % (.getName f)) (:out-file-patterns settings)))
    (or
     (empty? (:in-file-types settings))
     (contains? (:in-file-types settings) (get-file-type f)))
    (or
     (empty? (:out-file-types settings))
     (not (contains? (:out-file-types settings) (get-file-type f))))))

(defn filter-file? [f settings]
  (and
    (or
      (not (hidden-file? f))
      (not (:exclude-hidden settings)))
    (if (archive-file? f)
      (and
        (:search-archives settings)
        (is-archive-search-file? f settings))
      (and
        (not (:archives-only settings))
        (is-search-file? f settings)))))

(defn get-search-files-for-path [settings path]
  (let [pathfile (file path)]
    (if (.isFile pathfile)
      (vec
       (map
        #(new-search-file % (get-file-type %))
        (filter #(filter-file? % settings) [pathfile])))
      (if (:recursive settings)
        (vec
         (map
          #(new-search-file % (get-file-type %))
          (filter #(filter-file? % settings) (filter #(.isFile %) (file-seq pathfile)))))
        (vec
         (map
          #(new-search-file % (get-file-type %))
          (filter #(filter-file? % settings) (filter #(.isFile %) (.listFiles pathfile)))))))))

(defn get-search-files
  ([settings]
    (get-search-files settings (:paths settings) []))
  ([settings paths search-files]
    (if (empty? paths)
      search-files
      (let [nextsearch-files (get-search-files-for-path settings (first paths))]
        (get-search-files settings (rest paths) (concat search-files nextsearch-files))))))

(defn search-archive-file [f settings]
  (if (:verbose settings)
    (log-msg (format "Searching archive file %s" f))))

(defn search-binary-string-for-pattern
  ([b p settings]
    (let [m (re-matcher p b)]
      (if (.find m 0)
        (search-binary-string-for-pattern b m 0 settings)
        [])))
  ([b m i settings]
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

(defn search-binary-string [b settings]
  (if (:debug settings)
    (log-msg "Searching binary string"))
  (apply concat
    (map #(search-binary-string-for-pattern b % settings) (:search-patterns settings))))

(defn search-binary-file [sf settings]
  (if (:verbose settings)
    (log-msg (format "Searching binary file %s" (search-file-path sf))))
  (let [contents (slurp (:file sf) :encoding "ISO-8859-1") ; use single-byte enc to avoid corruption
        search-results (search-binary-string contents settings)
        with-file-results (map #(assoc-in % [:file] sf) search-results)]
    with-file-results))

(defn matches-any-pattern? [s pp]
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

(defn lines-before-match? [lines-before settings]
  (lines-match? lines-before (:in-lines-before-patterns settings) (:out-lines-before-patterns settings)))

(defn lines-after-match? [lines-after settings]
  (lines-match? lines-after (:in-lines-after-patterns settings) (:out-lines-after-patterns settings)))

(defn get-newline-indices [s]
  (map first 
    (filter #(= (second %) \newline)
      (map-indexed vector s))))

(defn get-multiline-lines-before [s beforestartindices beforeendindices settings]
  (if (> (:lines-before settings) 0)
    (let [lines-before (:lines-before settings)
          startindices (take-last lines-before beforestartindices)
          endindices (take-last lines-before beforeendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn get-multiline-lines-after [s afterstartindices afterendindices settings]
  (if (> (:lines-after settings) 0)
    (let [lines-after (:lines-after settings)
          startindices (take lines-after afterstartindices)
          endindices (take lines-after afterendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn search-multiline-string-for-pattern
  ([s p settings]
    (let [m (re-matcher p s)]
      (if (.find m 0)
        (let [newlineindices (get-newline-indices s)
              startlineindices (concat [0] (map inc newlineindices))
              endlineindices (concat newlineindices [(count s)])]
          (search-multiline-string-for-pattern s m 0 startlineindices
            endlineindices settings)))))
  ([s m i startlineindices endlineindices settings]
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

(defn search-multiline-string [s settings]
  (apply concat
    (map #(search-multiline-string-for-pattern s % settings) (:search-patterns settings))))

(defn search-text-file-contents [sf settings]
  (let [contents (slurp (:file sf) :encoding (:text-file-encoding settings))
        search-results (search-multiline-string contents settings)
        with-file-results (map #(assoc-in % [:file] sf) search-results)]
    with-file-results))

(defn search-line-for-pattern
  ([line-num line lines-before lines-after p settings]
    (let [m (re-matcher p line)]
      (if
        (and
          (.find m 0)
          (lines-before-match? lines-before settings)
          (lines-after-match? lines-after settings))
        (search-line-for-pattern line-num line lines-before lines-after m 0 [] settings)
        [])))
  ([line-num line lines-before lines-after m i results settings]
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

(defn search-line [line-num line lines-before lines-after settings]
  (apply concat
    (map #(search-line-for-pattern line-num line lines-before lines-after % settings)
      (:search-patterns settings))))

(defn search-lines
  ([lines settings]
    (let [line (first lines)
          nextlines (drop (:lines-after settings) (rest lines))
          lines-before []
          lines-after (take (:lines-after settings) (rest lines))]
      (search-lines 1 line nextlines lines-before lines-after [] settings)))
  ([line-num line lines lines-before lines-after results settings]
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

(defn search-text-file-lines [sf settings]
  (with-open [rdr (reader (:file sf) :encoding (:text-file-encoding settings))]
    (let [search-results (search-lines (line-seq rdr) settings)
          with-file-results (map #(assoc-in % [:file] sf) search-results)]
      with-file-results)))

(defn search-text-file [sf settings]
  (if (:verbose settings)
    (log-msg (format "Searching text file %s" (search-file-path sf))))
  (if (:multi-line-search settings)
    (search-text-file-contents sf settings)
    (search-text-file-lines sf settings)))

(defn search-file [sf settings]
  (let [file-type (:file-type sf)
        verbose (:verbose settings)
        file-path (search-file-path sf)]
    (cond
      (or
        (= file-type :code)
        (= file-type :text)
        (= file-type :xml)) (search-text-file sf settings)
      (= file-type :binary) (search-binary-file sf settings)
      (= file-type :archive)
        (if (:search-archives settings)
          (search-archive-file sf settings)
          (do
            (if verbose (log-msg (format "Skipping archive file %s" file-path))
            [])))
      :else
        (do
          (if verbose (log-msg (format "Skipping file of unknown type: %s" file-path))
          [])))))

(defn search-files [search-files settings]
  (if (:verbose settings)
    (do
      (log-msg (format "\nFiles to be searched (%d):" (count search-files)))
      (doseq [sf search-files] (log-msg (search-file-path sf)))
      (log-msg "")))
  (apply concat (map #(search-file % settings) search-files)))

(defn search [settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      [(search-files (get-search-files settings) settings) []]
      [[] errs])))
