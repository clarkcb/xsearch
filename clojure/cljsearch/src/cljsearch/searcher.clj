;;; search.clj: Recursive file search utility

;; by Cary Clark
;; April 10, 2010

(ns cljsearch.searcher
  #^{:author "Cary Clark",
     :doc "Recursive file search utility"}
  (:import (java.io File)
           (java.util.jar JarFile)
           (java.util.zip ZipFile))
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.string :as str :only (join trim)])
  (:use [cljsearch.common])
  (:use [cljsearch.filetypes])
  (:use [cljsearch.fileutil])
  (:use [cljsearch.searchresult])
)

; ref to contain the seq of SearchResult records
(def search-results (ref []))

(defn save-search-result
  "Saves a SearchResult to the search-results vector ref"
  [r]
  (dosync
    (alter search-results conj r)))

(defn is-search-dir [d settings]
  (let [in-dirpatterns (:in-dirpatterns settings)
        out-dirpatterns (:out-dirpatterns settings)]
    (or
      (is-dot-dir? (get-name d))
      (and
        (or
          (not (:excludehidden settings))
          (not (hidden-dir? d)))
        (or
          (empty? in-dirpatterns)
          (some #(re-find % (.getPath d)) in-dirpatterns))
        (or
          (empty? out-dirpatterns)
          (not-any? #(re-find % (.getPath d)) out-dirpatterns))))))

(defn print-search-result [r]
  (log-msg (search-result-to-string r)))

(defn print-search-results []
  (log-msg (format"\nSearch results (%d):" (count (deref search-results))))
  (doseq [r (deref search-results)] (print-search-result r)))

(defn get-matching-dirs []
  (sort (distinct (map #(.getParent (:file %)) (deref search-results)))))

(defn print-matching-dirs []
  (let [dirs (get-matching-dirs)]
    (log-msg (format"\nDirectories with matches (%d):" (count dirs)))
    (doseq [d dirs] (log-msg d))))

(defn get-matching-files []
  (sort (distinct (map #(.getPath (:file %)) (deref search-results)))))

(defn print-matching-files []
  (let [files (get-matching-files)]
    (log-msg (format"\nFiles with matches (%d):" (count files)))
    (doseq [f files] (log-msg f))))

(defn get-matching-lines [settings]
  (let [lines (sort (map #(str/trim (:line %)) (deref search-results)))]
    (if (:uniquelines settings)
      (distinct lines)
      lines)))

(defn print-matching-lines [settings]
  (let [lines (get-matching-lines settings)]
    (log-msg
      (if (:uniquelines settings)
        (format"\nUnique lines with matches (%d):" (count lines))
        (format"\nLines with matches (%d):" (count lines))))
    (doseq [l lines] (log-msg l))))

(defn validate-settings [settings]
  (let [startpath (:startpath settings)
        startdir (file startpath)
        tests [(fn [ss] (if (empty? startpath) "Startpath not defined" nil))
               (fn [ss] (if (not (.exists startdir)) "Startpath not found" nil))
               (fn [ss] (if (not (is-search-dir startdir ss)) "Startpath does not match settings" nil))
               (fn [ss] (if (empty? (:searchpatterns ss)) "No search patterns specified" nil))]]
    (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))

(defn get-search-dirs [settings]
  (let [startdir (file (:startpath settings))]
    (if (:recursive settings)
      (vec (filter #(is-search-dir % settings) (filter #(.isDirectory %) (file-seq startdir))))
      [startdir])))

(defn is-archive-search-file [f settings]
  (let [in-extensions (:in-archiveextensions settings)
        out-extensions (:out-archiveextensions settings)
        in-filepatterns (:in-archivefilepatterns settings)
        out-filepatterns (:out-archivefilepatterns settings)]
    (and
      (or
        (empty? in-extensions)
        (some #(= % (get-ext f)) in-extensions))
      (or
        (empty? out-extensions)
        (not-any? #(= % (get-ext f)) out-extensions))
      (or
        (empty? in-filepatterns)
        (some #(re-find % (.getName f)) in-filepatterns))
      (or
        (empty? out-filepatterns)
        (not-any? #(re-find % (.getName f)) out-filepatterns)))))

(defn is-search-file [f settings]
  (let [in-extensions (:in-extensions settings)
        out-extensions (:out-extensions settings)
        in-filepatterns (:in-filepatterns settings)
        out-filepatterns (:out-filepatterns settings)]
    (and
      (or
        (empty? in-extensions)
        (some #(= % (get-ext f)) in-extensions))
      (or
        (empty? out-extensions)
        (not-any? #(= % (get-ext f)) out-extensions))
      (or
        (empty? in-filepatterns)
        (some #(re-find % (.getName f)) in-filepatterns))
      (or
        (empty? out-filepatterns)
        (not-any? #(re-find % (.getName f)) out-filepatterns)))))

(defn filter-file [f settings]
  (and
    (or
      (not (:excludehidden settings))
      (not (hidden-file? f)))
    (or
      (and
        (archive-file? f)
        (:searcharchives settings)
        (is-archive-search-file f settings))
      (and
        (not (:archivesonly settings))
        (is-search-file f settings)))))

(defn get-search-files-for-directory [d settings]
  (vec (filter #(filter-file % settings) (get-files-in-directory d))))

(defn get-search-files [searchdirs settings]
  (apply concat (map #(get-search-files-for-directory % settings) searchdirs)))

(defn search-archive-file [f settings]
  (if (:verbose settings)
    (log-msg (format "Searching archive file %s" f))
  )
)

(defn search-binary-string-for-pattern [b p settings]
  (if (:debug settings)
    (log-msg (format "Searching binary string for pattern %s" p)))
  (if (re-find p b)
    [(->SearchResult p nil 0 0 0 "" [] [])]
    []))

(defn search-binary-string [b settings]
  (if (:debug settings)
    (log-msg "Searching binary string"))
  (apply concat (map #(search-binary-string-for-pattern b % settings) (:searchpatterns settings))))

(defn search-binary-file [f settings]
  (if (:verbose settings)
    (log-msg (format "Searching binary file %s" f)))
  (let [contents (slurp f)
        search-results (search-binary-string contents settings)
        with-file-results (map #(assoc-in % [:file] f) search-results)]
    (doseq [r with-file-results] (save-search-result r))))

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
    (if (:debug settings)
      (log-msg (format "Searching multi-line string for pattern %s" p)))
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
        (if (:debug settings)
          (log-msg (format "\nFound match for pattern %s in multi-line string"
            (.pattern m))))
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
          ;(log-msg (format "startmatchindex: %d" startmatchindex))
          ;(log-msg (format "endmatchindex: %d" endmatchindex))
          ;(log-msg (format "startlineindices (%d): (%s)" (count startlineindices) (str/join "," startlineindices)))
          ;(log-msg (format "endlineindices (%d): (%s)" (count endlineindices) (str/join "," endlineindices)))
          ;(log-msg (format "beforestartindices (%d): (%s)" (count beforestartindices) (str/join "," beforestartindices)))
          ;(log-msg (format "beforeendindices (%d): (%s)" (count beforeendindices) (str/join "," beforeendindices)))
          ;(log-msg (format "afterstartindices (%d): (%s)" (count afterstartindices) (str/join "," afterstartindices)))
          ;(log-msg (format "afterendindices (%d): (%s)" (count afterendindices) (str/join "," afterendindices)))
          ;(log-msg (format "startlineindex: %d" startlineindex))
          ;(log-msg (format "endlineindex: %d" endlineindex))
          ;(log-msg (format "line: \"%s\"" line))
          ;(log-msg (format "linenum: %d" linenum))
          (if (:firstmatch settings)
            [result]
            (concat [result] (search-multiline-string-for-pattern s m
              endmatchindex startlineindices endlineindices settings)))))
      [])))

(defn search-multiline-string [s settings]
  (if (:debug settings)
    (log-msg "Searching multi-line string"))
  (apply concat (map #(search-multiline-string-for-pattern s % settings) (:searchpatterns settings))))

(defn search-text-file-contents [f settings]
  (if (:debug settings)
    (log-msg (format "Searching text file contents: %s" f)))
  (let [contents (slurp f)
        search-results (search-multiline-string contents settings)
        with-file-results (map #(assoc-in % [:file] f) search-results)]
    (doseq [r with-file-results] (save-search-result r))))

(defn search-text-file-lines [f settings]
  (if (:debug settings)
    (log-msg (format "Searching text file lines: %s" f)))
  ;; TODO
)

(defn search-text-file [f settings]
  (if (:verbose settings)
    (log-msg (format "Searching text file %s" f)))
  (if (:multilinesearch settings)
    (search-text-file-contents f settings)
    (search-text-file-lines f settings)))

(defn search-file [f settings]
  (let [filetype (get-filetype f)
        verbose (:verbose settings)]
    (cond
      (= filetype :text) (search-text-file f settings)
      (= filetype :binary) (search-binary-file f settings)
      (= filetype :archive)
        (if (:searcharchives settings)
          (search-archive-file f settings)
          (if verbose (log-msg (format "Skipping archive file %s" f))))
      :else
        (if verbose (log-msg (format "Skipping file of unknown type: %s" f))))))

(defn search-files [searchfiles settings]
  (if (:verbose settings)
    (do
      (log-msg (format "\nFiles to be searched (%d):" (count searchfiles)))
      (doseq [f searchfiles] (log-msg (.getPath f)))
      (log-msg "")))
  (doseq [f searchfiles] (search-file f settings)))

(defn search-dirs [searchdirs settings]
  (if (:verbose settings)
    (do
      (log-msg (format "\nDirectories to be searched (%d):" (count searchdirs)))
      (doseq [d searchdirs] (log-msg (.getPath d)))))
  (search-files (get-search-files searchdirs settings) settings))

(defn search [settings]
  (let [errs (validate-settings settings)]
    (if (not (empty? errs))
      (log-errors errs)
      (search-dirs (get-search-dirs settings) settings))))
