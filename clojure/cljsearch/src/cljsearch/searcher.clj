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
        [cljsearch.filetypes :only (archive-file? get-filetype)]
        [cljsearch.fileutil :only
          (get-ext get-files-in-directory get-name hidden-dir? hidden-file?
            is-dot-dir?)]
        [cljsearch.searchfile :only (new-search-file search-file-path)]
        [cljsearch.searchresult :only
          (->SearchResult search-result-to-string)]))

(defn is-search-dir? [d settings]
  (or
    (is-dot-dir? (get-name d))
    (and
      (or
        (not (:excludehidden settings))
        (not (hidden-dir? d)))
      (or
       (empty? (:in-dirpatterns settings))
        (some #(re-find % (.getPath d)) (:in-dirpatterns settings)))
      (or
       (empty? (:out-dirpatterns settings))
        (not-any? #(re-find % (.getPath d)) (:out-dirpatterns settings))))))

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
    (if (:uniquelines settings)
      (distinct lines)
      lines)))

(defn print-matching-lines [results settings]
  (let [lines (get-matching-lines results settings)]
    (log-msg
      (if (:uniquelines settings)
        (format "\nUnique lines with matches (%d):" (count lines))
        (format "\nLines with matches (%d):" (count lines))))
    (doseq [l lines] (log-msg l))))

(defn validate-path [path]
  (if (not path)
    "Startpath not defined"
    (let [filepath (if path (file path) nil)]
      (if (or (not filepath) (not (.exists filepath)))
        "Startpath not found"
        (if (not (.canRead filepath))
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
      (let [tests [(fn [ss] (if (empty? (:searchpatterns ss)) "No search patterns defined" nil))
                   (fn [ss]
                     (if
                       (not
                         (=
                           (try
                             (java.nio.charset.Charset/forName (:textfileencoding ss))
                             (catch IllegalArgumentException e nil))
                           nil)
                       ) nil (format "Invalid encoding: %s" (:textfileencoding ss))))
                   (fn [ss] (if (< (:linesafter ss) 0) "Invalid linesafter" nil))
                   (fn [ss] (if (< (:linesbefore ss) 0) "Invalid linesbefore" nil))
                   (fn [ss] (if (< (:maxlinelength ss) 0) "Invalid maxlinelength" nil))
                  ]]
        (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))))

(defn is-archive-search-file? [f settings]
  (and
    (or
     (empty? (:in-archiveextensions settings))
     (some #(= % (get-ext f)) (:in-archiveextensions settings)))
    (or
     (empty? (:out-archiveextensions settings))
     (not-any? #(= % (get-ext f)) (:out-archiveextensions settings)))
    (or
     (empty? (:in-archivefilepatterns settings))
     (some #(re-find % (.getName f)) (:in-archivefilepatterns settings)))
    (or
     (empty? (:out-archivefilepatterns settings))
     (not-any? #(re-find % (.getName f)) (:out-archivefilepatterns settings)))))

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
     (empty? (:in-filepatterns settings))
     (some #(re-find % (.getName f)) (:in-filepatterns settings)))
    (or
     (empty? (:out-filepatterns settings))
     (not-any? #(re-find % (.getName f)) (:out-filepatterns settings)))
    (or
     (empty? (:in-filetypes settings))
     (contains? (:in-filetypes settings) (get-filetype f)))
    (or
     (empty? (:out-filetypes settings))
     (not (contains? (:out-filetypes settings) (get-filetype f))))))

(defn filter-file? [f settings]
  (and
    (or
      (not (hidden-file? f))
      (not (:excludehidden settings)))
    (if (archive-file? f)
      (and
        (:searcharchives settings)
        (is-archive-search-file? f settings))
      (and
        (not (:archivesonly settings))
        (is-search-file? f settings)))))

(defn get-search-files-for-path [settings path]
  (let [pathfile (file path)]
    (if (.isFile pathfile)
      (vec
       (map
        #(new-search-file % (get-filetype %))
        (filter #(filter-file? % settings) [pathfile])))
      (if (:recursive settings)
        (vec
         (map
          #(new-search-file % (get-filetype %))
          (filter #(filter-file? % settings) (filter #(.isFile %) (file-seq pathfile)))))
        (vec
         (map
          #(new-search-file % (get-filetype %))
          (filter #(filter-file? % settings) (filter #(.isFile %) (.listFiles pathfile)))))))))

(defn get-search-files
  ([settings]
    (get-search-files settings (:paths settings) []))
  ([settings paths searchfiles]
    (if (empty? paths)
      searchfiles
      (let [nextsearchfiles (get-search-files-for-path settings (first paths))]
        (get-search-files settings (rest paths) (concat searchfiles nextsearchfiles))))))

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
          (if (:firstmatch settings)
            [result]
            (concat [result] (search-binary-string-for-pattern b m
              endmatchindex settings)))))
      [])))

(defn search-binary-string [b settings]
  (if (:debug settings)
    (log-msg "Searching binary string"))
  (apply concat
    (map #(search-binary-string-for-pattern b % settings) (:searchpatterns settings))))

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

(defn linesmatch? [lines inpatterns outpatterns]
  (and
    (or
      (empty? inpatterns)
      (any-matches-any-pattern? lines inpatterns))
    (or
      (empty? outpatterns)
      (not (any-matches-any-pattern? lines outpatterns)))))

(defn linesbefore-match? [linesbefore settings]
  (linesmatch? linesbefore (:in-linesbeforepatterns settings) (:out-linesbeforepatterns settings)))

(defn linesafter-match? [linesafter settings]
  (linesmatch? linesafter (:in-linesafterpatterns settings) (:out-linesafterpatterns settings)))

(defn get-newline-indices [s]
  (map first 
    (filter #(= (second %) \newline)
      (map-indexed vector s))))

(defn get-multiline-linesbefore [s beforestartindices beforeendindices settings]
  (if (> (:linesbefore settings) 0)
    (let [linesbefore (:linesbefore settings)
          startindices (take-last linesbefore beforestartindices)
          endindices (take-last linesbefore beforeendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn get-multiline-linesafter [s afterstartindices afterendindices settings]
  (if (> (:linesafter settings) 0)
    (let [linesafter (:linesafter settings)
          startindices (take linesafter afterstartindices)
          endindices (take linesafter afterendindices)]
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
              linenum (count beforestartindices)
              linesbefore (get-multiline-linesbefore s (butlast beforestartindices)
                beforeendindices settings)
              afterstartindices (filter #(> % startmatchindex) startlineindices)
              afterendindices (filter #(> % startmatchindex) endlineindices)
              linesafter (get-multiline-linesafter s afterstartindices
                (rest afterendindices) settings)
              result (->SearchResult
                       (.pattern m)
                       nil 
                       linenum
                       (+ (- startmatchindex startlineindex) 1)
                       (+ (- endmatchindex startlineindex) 1)
                       line
                       linesbefore
                       linesafter)]
          (if
            (and
              (or
                (= (:linesbefore settings) 0)
                (linesbefore-match? linesbefore settings))
              (or
                (= (:linesafter settings) 0)
                (linesafter-match? linesafter settings)))
            (if (:firstmatch settings)
              [result]
              (concat [result] (search-multiline-string-for-pattern s m
                endmatchindex startlineindices endlineindices settings)))
            [])))
      [])))

(defn search-multiline-string [s settings]
  (apply concat
    (map #(search-multiline-string-for-pattern s % settings) (:searchpatterns settings))))

(defn search-text-file-contents [sf settings]
  (let [contents (slurp (:file sf) :encoding (:textfileencoding settings))
        search-results (search-multiline-string contents settings)
        with-file-results (map #(assoc-in % [:file] sf) search-results)]
    with-file-results))

(defn search-line-for-pattern
  ([linenum line linesbefore linesafter p settings]
    (let [m (re-matcher p line)]
      (if
        (and
          (.find m 0)
          (linesbefore-match? linesbefore settings)
          (linesafter-match? linesafter settings))
        (search-line-for-pattern linenum line linesbefore linesafter m 0 [] settings)
        [])))
  ([linenum line linesbefore linesafter m i results settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              result (->SearchResult
                       (.pattern m)
                       nil 
                       linenum
                       (+ startmatchindex 1)
                       (+ endmatchindex 1)
                       line
                       linesbefore
                       linesafter)]
          (search-line-for-pattern linenum line linesbefore linesafter m
            endmatchindex (concat results [result]) settings)))
      results)))

(defn search-line [linenum line linesbefore linesafter settings]
  (apply concat
    (map #(search-line-for-pattern linenum line linesbefore linesafter % settings)
      (:searchpatterns settings))))

(defn search-lines
  ([lines settings]
    (let [line (first lines)
          nextlines (drop (:linesafter settings) (rest lines))
          linesbefore []
          linesafter (take (:linesafter settings) (rest lines))]
      (search-lines 1 line nextlines linesbefore linesafter [] settings)))
  ([linenum line lines linesbefore linesafter results settings]
    (if line
      (let [nextresults (search-line linenum line linesbefore linesafter settings)
            nextlinenum (+ linenum 1)
            nextline (if (empty? linesafter) (first lines) (first linesafter))
            nextlinesbefore
              (if (> (:linesbefore settings) 0)
                (if (= (count linesbefore) (:linesbefore settings))
                  (concat (rest linesbefore) [line])
                  (concat linesbefore [line]))
                [])
            nextlinesafter
              (if (> (:linesafter settings) 0)
                (concat (rest linesafter) (take 1 lines))
                [])]
          (search-lines nextlinenum nextline (rest lines) nextlinesbefore
            nextlinesafter (concat results nextresults) settings))
      (if
        (and
          (> (count results) 0)
          (:firstmatch settings))
        (take 1 results)
        results))))

(defn search-text-file-lines [sf settings]
  (with-open [rdr (reader (:file sf) :encoding (:textfileencoding settings))]
    (let [search-results (search-lines (line-seq rdr) settings)
          with-file-results (map #(assoc-in % [:file] sf) search-results)]
      with-file-results)))

(defn search-text-file [sf settings]
  (if (:verbose settings)
    (log-msg (format "Searching text file %s" (search-file-path sf))))
  (if (:multilinesearch settings)
    (search-text-file-contents sf settings)
    (search-text-file-lines sf settings)))

(defn search-file [sf settings]
  (let [filetype (:filetype sf)
        verbose (:verbose settings)
        filepath (search-file-path sf)]
    (cond
      (or
        (= filetype :code)
        (= filetype :text)
        (= filetype :xml)) (search-text-file sf settings)
      (= filetype :binary) (search-binary-file sf settings)
      (= filetype :archive)
        (if (:searcharchives settings)
          (search-archive-file sf settings)
          (do
            (if verbose (log-msg (format "Skipping archive file %s" filepath))
            [])))
      :else
        (do
          (if verbose (log-msg (format "Skipping file of unknown type: %s" filepath))
          [])))))

(defn search-files [searchfiles settings]
  (if (:verbose settings)
    (do
      (log-msg (format "\nFiles to be searched (%d):" (count searchfiles)))
      (doseq [sf searchfiles] (log-msg (search-file-path sf)))
      (log-msg "")))
  (apply concat (map #(search-file % settings) searchfiles)))

(defn search [settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      [(search-files (get-search-files settings) settings) []]
      [[] errs])))
