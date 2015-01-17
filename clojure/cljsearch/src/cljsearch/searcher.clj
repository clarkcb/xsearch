;;; search.clj: Recursive file search utility

;; by Cary Clark
;; April 10, 2010

(ns cljsearch.searcher
  #^{:author "Cary Clark",
     :doc "Recursive file search utility"}
  (:import (java.io File)
           (java.util.jar JarFile)
           (java.util.zip ZipFile))
  (:use [clojure.java.io :only (reader)])
  (:use [elocale.file :as file])
  (:use [elocale.utils :as utils]))

(defn printlines? [opts]
  (get opts :p))

(defn print-search-result
  "Prints a search-result"
  [file line linenum pattern opts]
  (if (> (count (get opts :s)) 1)
    (print (str pattern ": ")))
  (if (> linenum 0)
    (println (format "%s: %d: %s" (get-path file) linenum (.trim line)))
    (println (format "%s has match" (get-path file)))))

(defn save-search-result
  "Creates a search-result struct and conjoins it to the search-results vector ref"
  [file line linenum pattern opts]
  (dosync
    (alter search-results conj (struct search-result file line linenum pattern)))
  (if (printlines? opts)
    (print-search-result file line linenum pattern opts)))

(defn search-text-file-line [file line linenum opts]
  (if line
    (loop [patterns (get opts :s)]
      (if (> (count (re-seq (first patterns) line)) 0)
        (save-search-result file line linenum (first patterns) opts))
      (if (second patterns)
        (recur
          (rest patterns))))))

(defn search-reader [rdr file opts]
  (loop [lines (line-seq rdr)
         linenum 1]
    (search-text-file-line file (first lines) linenum opts)
    (if (second lines)
      (recur
        (rest lines)
        (inc linenum)))))

(defn search-text-file [file opts]
  ;(println "search-text-file" file)
  (with-open [rdr (get-reader file)]
    (search-reader rdr (get-path file) opts)))

(defn search-binary-file [file opts]
  ;(println "search-binary-file" file)
  (let [contents (get-contents file)]
    (loop [patterns (get opts :s)]
      (if (> (count (re-seq (first patterns) contents)) 0)
        (save-search-result file "" 0 (first patterns) opts))
      (if (second patterns)
        (recur
          (rest patterns))))))

(defn search-binary? [opts]
  (get opts :b))

(defn get-zip-file-entries [zipfile opts]
  (let [entries (filter #(not (.isDirectory %)) (enumeration-seq (.entries zipfile)))]
    (filter-out-by-ext (concat NOSEARCH-EXTS (get opts :X))
      (filter-in-by-ext (get opts :x) entries))))

(defn search-zip-file [zipfile opts]
  (doseq [zipentry (get-zip-file-entries zipfile opts)]
    (if (text-file? zipentry)
      (with-open [rdr (reader (.getInputStream zipfile zipentry))]
        (search-reader rdr (str (.getName zipfile) "[" (.getName zipentry) "]") opts)))))

(defn search-compressed-file [file opts]
  (cond (has-ext? "zip" file) (search-zip-file (ZipFile. file) opts)
        (has-ext? "jar" file) (search-zip-file (JarFile. file) opts)))

(defn search-compressed? [opts]
  (get opts :z))

(defn search-file [file opts]
  (cond (text-file? file) (search-text-file file opts)
        (compressed-file? file) (search-compressed-file file opts)
        ;(binary-file? file) (search-binary-file file opts)))
        :else (if-not (no-search-file? file) (search-binary-file file opts))))

(defn get-exclude-exts [opts]
  (let [exclude-exts (set (get opts :X))]
    (if (> (count exclude-exts) 0)
      (set
        (concat
          exclude-exts
          NOSEARCH-EXTS
          (if (search-binary? opts)
            #{}
            BINARY-EXTS)
          (if (search-compressed? opts)
            #{}
            COMPRESSED-EXTS)))
      #{})))

(defn get-include-exts [opts]
  (let [include-exts (set (get opts :x))]
    (if (> (count include-exts) 0)
      (set
        (concat
          include-exts
          (if (search-compressed? opts)
            COMPRESSED-EXTS
            #{})))
      #{})))

(defn filtered-files [startdir opts]
  (let [files (filter #(file? %) (get-file-seq startdir))
        exclude-exts (get-exclude-exts opts)
        include-exts (get-include-exts opts)]
    ;(println "exclude-exts: " exclude-exts)
    ;(println "include-exts: " include-exts)
    (filter-out-by-ext exclude-exts
      (filter-in-by-ext include-exts
        (filter-out-by-pattern (get opts :F)
          (filter-in-by-pattern (get opts :f) files))))))

(defn print-search-results [opts]
  (loop [patterns (get opts :s)]
    (println
      (format
        "Matches for \"%s\": %d"
        (first patterns)
        (count (for [result @search-results :when (= (:pattern result) (first patterns))] result))))
    (if (second patterns)
      (recur
        (rest patterns)))))

(defn search [startdir opts]
  (doseq [file (filtered-files startdir opts)]
    (search-file file opts))
  (print-search-results opts))

(defn indexed [coll] (map vector (iterate inc 0) coll))

(defn get-args-for-opt [coll o]
  (if ((set coll) o)
    (vec
      (set
        (for [[i elem] (indexed coll)
              :when (and (= elem o) (> (count coll) (+ i 1)))]
          (nth coll (inc i)))))
    []))

(defn get-opts [coll]
  (let [a (not (contains? (set coll) "-1"))   ; flag for all matches vs. first match only per file
        b (not (contains? (set coll) "-B"))   ; flag for searching binary files
        f (vec (map #(re-pattern %) (get-args-for-opt coll "-f"))) ; inclusive file name patterns
        F (vec (map #(re-pattern %) (get-args-for-opt coll "-F"))) ; exclusive file name patterns
        filelist (contains? (set coll) "--filelist")   ; flag for listing the files with matches
        linelist (contains? (set coll) "--linelist")   ; flag for listing the lines with matches
        p (not (contains? (set coll) "-P"))   ; flag for printing results as found
        s (vec (map #(re-pattern %) (get-args-for-opt coll "-s"))) ; search strings
        x (get-args-for-opt coll "-x")        ; the extensions to include
        X (get-args-for-opt coll "-X")        ; the extensions to exclude
        z (not (contains? (set coll) "-Z"))]  ; flag for searching compressed files
    {:a a, :b b, :f f, :F F, :filelist filelist, :linelist linelist, :p p, :s s, :x x, :X X, :z z}))

(defn print-linelist
  "print the unique list of lines with matches"
  []
  (let [lines (sort (set (for [result @search-results :when (not (= (get result :line) ""))] (get result :line))))]
    (println (format "Lines with matches (%d lines):" (count lines)))
    (doseq [line lines]
      (println line))))

(defn print-filelist
  "print the unique list of files with matches"
  []
  (let [files (sort (set (for [result @search-results] (get result :file))))]
    (println (format "Files with matches (%d files):" (count files)))
    (doseq [file files]
      (println file))))

(defn has-errors [opts startdir]
  (cond
    (< (count (get opts :s)) 1) true
    (contains? (set (concat (vals opts))) startdir) true
    :else false))

(defn main []
  (if (or (nil? *command-line-args*) (< (count *command-line-args*) 3))
    (usage))
  (let [opts (get-opts (butlast *command-line-args*))
        startdir (last *command-line-args*)]
    ;(println (format "*command-line-args*: %s" *command-line-args*))
    (println (format "opts: %s" opts))
    ;(println (format "startdir: %s" startdir))
    (if (has-errors opts startdir)
      (usage))
    (search startdir opts)
    (if (get opts :filelist)
      (print-filelist))
    (if (get opts :linelist)
      (print-linelist))))

(time (main))
