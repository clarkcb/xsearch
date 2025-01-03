;;; ############################################################################
;;;
;;; search-options.clj
;;;
;;; Defines the available command-line options and utility functions
;;;
;;; ############################################################################

(ns cljsearch.searchoptions
  #^{:author "Cary Clark",
     :doc "Defines the available command-line options utility functions"}
  (:import (java.io File))
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [cljsearch.searchsettings])
  (:import (cljsearch.searchsettings SearchSettings))
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileutil :only (exists? expand-path path-str to-path)]
        [cljfind.findsettings :only
         (add-extension add-file-type add-path add-pattern
          set-archives-only set-debug set-int-val set-long-val
          sort-by-from-name)]
        [cljsearch.searchsettings :only
         (->SearchSettings DEFAULT-SEARCH-SETTINGS)]))

(defrecord SearchOption [short-arg long-arg desc])

(defn get-sort-arg ^String [^SearchOption so]
  (if (= "" (:short-arg so))
    (:long-arg so)
    (str (str/lower-case (:short-arg so)) "a" (:long-arg so))))

(defn get-search-options-from-json []
  (let [contents (slurp (io/resource "searchoptions.json"))
        search-options-objs (:searchoptions (json/read-str contents :key-fn keyword))
        long-names (map #(get % :long) search-options-objs)
        short-names (map #(get % :short "") search-options-objs)
        long-short-map (zipmap long-names short-names)
        descs (map #(.trim %) (map :desc search-options-objs))
        long-desc-map (zipmap long-names descs)
        get-short (fn [l] (get long-short-map l))
        get-desc (fn [l] (get long-desc-map l))]
    (sort-by get-sort-arg (map #(SearchOption. (get-short %) % (get-desc %)) long-names))))

(def ^:const SEARCH-OPTIONS (get-search-options-from-json))

(defn print-option [^SearchOption opt]
  (let [format-string "(SearchOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o SEARCH-OPTIONS] (print-option o)))

(def bool-action-map
  { :allmatches (fn [^SearchSettings settings b] (assoc settings :first-match (not b)))
    :archivesonly (fn [^SearchSettings settings b] (set-archives-only settings b))
    :colorize (fn [^SearchSettings settings b] (assoc settings :colorize b))
    :debug (fn [^SearchSettings settings b] (set-debug settings b))
    :excludehidden (fn [^SearchSettings settings b] (assoc settings :exclude-hidden b))
    :firstmatch (fn [^SearchSettings settings b] (assoc settings :first-match b))
    :followsymlinks (fn [^SearchSettings settings b] (assoc settings :follow-symlinks b))
    :help (fn [^SearchSettings settings b] (assoc settings :print-usage b))
    :includehidden (fn [^SearchSettings settings b] (assoc settings :exclude-hidden (not b)))
    :multilinesearch (fn [^SearchSettings settings b] (assoc settings :multi-line-search b))
    :nocolorize (fn [^SearchSettings settings b] (assoc settings :colorize (not b)))
    :nofollowsymlinks (fn [^SearchSettings settings b] (assoc settings :follow-symlinks (not b)))
    :noprintmatches (fn [^SearchSettings settings b] (assoc settings :print-results (not b)))
    :norecursive (fn [^SearchSettings settings b] (assoc settings :recursive (not b)))
    :nosearcharchives (fn [^SearchSettings settings b] (assoc settings :search-archives (not b)))
    :printdirs (fn [^SearchSettings settings b] (assoc settings :print-dirs b))
    :printfiles (fn [^SearchSettings settings b] (assoc settings :print-files b))
    :printlines (fn [^SearchSettings settings b] (assoc settings :print-lines b))
    :printmatches (fn [^SearchSettings settings b] (assoc settings :print-results b))
    :recursive (fn [^SearchSettings settings b] (assoc settings :recursive b))
    :searcharchives (fn [^SearchSettings settings b] (assoc settings :search-archives b))
    :sort-ascending (fn [^SearchSettings settings b] (assoc settings :sort-descending (not b)))
    :sort-caseinsensitive (fn [^SearchSettings settings b] (assoc settings :sort-case-insensitive b))
    :sort-casesensitive (fn [^SearchSettings settings b] (assoc settings :sort-case-insensitive (not b)))
    :sort-descending (fn [^SearchSettings settings b] (assoc settings :sort-descending b))
    :uniquelines (fn [^SearchSettings settings b] (assoc settings :unique-lines b))
    :verbose (fn [^SearchSettings settings b] (assoc settings :verbose b))
    :version (fn [^SearchSettings settings b] (assoc settings :version b))
  })

(def string-action-map
  { :encoding (fn [^SearchSettings settings ^String s] (assoc settings :text-file-encoding s))
    :in-archiveext (fn [^SearchSettings settings ^String s] (add-extension settings s :in-archive-extensions))
    :in-archivefilepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-archive-file-patterns))
    :in-dirpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-dir-patterns))
    :in-ext (fn [^SearchSettings settings ^String s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-file-patterns))
    :in-filetype (fn [^SearchSettings settings ^String s] (add-file-type settings s :in-file-types))
    :in-linesafterpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-lines-after-patterns))
    :in-linesbeforepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-lines-before-patterns))
    :linesaftertopattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :lines-after-to-patterns))
    :linesafteruntilpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :lines-after-until-patterns))
    :maxlastmod (fn [^SearchSettings settings ^String s] (assoc settings :max-last-mod (clojure.instant/read-instant-date s)))
    :minlastmod (fn [^SearchSettings settings ^String s] (assoc settings :min-last-mod (clojure.instant/read-instant-date s)))
    :out-archiveext (fn [^SearchSettings settings ^String s] (add-extension settings s :out-archive-extensions))
    :out-archivefilepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :out-archive-file-patterns))
    :out-dirpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :out-dir-patterns))
    :out-ext (fn [^SearchSettings settings ^String s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :out-file-patterns))
    :out-filetype (fn [^SearchSettings settings ^String s] (add-file-type settings s :out-file-types))
    :out-linesafterpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :out-lines-after-patterns))
    :out-linesbeforepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :out-lines-before-patterns))
    :path (fn [^SearchSettings settings ^String s] (add-path settings (to-path s)))
    :searchpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :search-patterns))
    :sort-by (fn [^SearchSettings settings ^String s] (assoc settings :sort-by (sort-by-from-name s)))
  })

(def int-action-map
  { :linesafter (fn [^SearchSettings settings i] (set-int-val settings i :lines-after))
    :linesbefore (fn [^SearchSettings settings i] (set-int-val settings i :lines-before))
    :maxdepth (fn [^SearchSettings settings i] (set-int-val settings i :max-depth))
    :maxlinelength (fn [^SearchSettings settings i] (set-int-val settings i :max-line-length))
    :mindepth (fn [^SearchSettings settings i] (set-int-val settings i :min-depth))
  })

(def long-action-map
  { :maxsize (fn [^SearchSettings settings l] (set-long-val settings l :max-size))
    :minsize (fn [^SearchSettings settings ^String s] (set-long-val settings s :min-size))
  })

(defn get-long-arg-map []
  (let [long-names     (concat (map :long-arg SEARCH-OPTIONS) ["path"])
        long-map       (zipmap long-names (map #(keyword %) long-names))
        short-options  (remove #(= (:short-arg %) "") SEARCH-OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map #(keyword %) (map :long-arg short-options)))]
    (merge long-map short-long-map)))

(defn get-long-arg [arg]
  (let [long-names (map :long-arg SEARCH-OPTIONS)
        long-map (zipmap long-names (repeat 1))
        short-options (remove #(= (:short-arg %) "") SEARCH-OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map :long-arg short-options))]
    (cond
      (contains? long-map arg) (keyword arg)
      (contains? short-long-map arg) (keyword (get short-long-map arg))
      :else nil)))

(defn settings-from-map [^SearchSettings settings ks m errs]
  (if (or (empty? ks) (not (empty? errs)))
    [settings errs]
    (let [k (keyword (first ks))
          v (k m)]
      (cond
        (contains? bool-action-map k)
          (if (instance? Boolean v)
            (settings-from-map ((k bool-action-map) settings v) (rest ks) m errs)
            (settings-from-map settings (rest ks) m (conj errs (str "Invalid value for option: " (name k)))))
        (contains? string-action-map k)
          (if (instance? String v)
            (settings-from-map ((k string-action-map) settings v) (rest ks) m errs)
            (if (coll? v)
              (if (empty? v)
                (settings-from-map settings (rest ks) m errs)
                (settings-from-map ((k string-action-map) settings (first v)) ks (assoc m k (rest v)) errs))
              (settings-from-map settings (rest ks) m (conj errs (str "Invalid value for option: " (name k))))))
        (contains? int-action-map k)
          (if (instance? Integer v)
            (settings-from-map ((k int-action-map) settings v) (rest ks) m errs)
            (if (instance? Long v)
              (settings-from-map ((k int-action-map) settings (.intValue v)) (rest ks) m errs)
              (settings-from-map settings (rest ks) m (conj errs (str "Invalid value for option: " (name k))))))
        (contains? long-action-map k)
          (if (instance? Integer v)
            (settings-from-map ((k long-action-map) settings (.longValue v)) (rest ks) m errs)
            (if (instance? Long v)
              (settings-from-map ((k long-action-map) settings v) (rest ks) m errs)
              (settings-from-map settings (rest ks) m (conj errs (str "Invalid value for option: " (name k))))))
        :else
          (settings-from-map settings (rest ks) m (conj errs (str "Invalid option: " k)))))))

(defn settings-from-json
  ([^String json]
   (settings-from-json DEFAULT-SEARCH-SETTINGS (get-long-arg-map) json))
  ([^SearchSettings settings long-arg-map ^String json]
    (let [obj (json/read-str json :key-fn keyword)
          ;; keys are sorted so that output is consistent across all versions
          ks (sort (keys obj))
          invalid-ks (remove #(contains? long-arg-map %) (map #(name %) ks))]
      (if (not (empty? invalid-ks))
        [nil [(str "Invalid option: " (first invalid-ks))]]
        (settings-from-map settings ks obj [])))))

(defn settings-from-file
  ([f]
   (settings-from-file DEFAULT-SEARCH-SETTINGS (get-long-arg-map) f))
  ([^SearchSettings settings long-arg-map f]
   (let [filepath (expand-path (to-path f))
         pathstr (path-str filepath)]
     (if (not (exists? filepath))
       [settings [(str "Settings file not found: " pathstr)]]
       (if (not (.endsWith pathstr ".json"))
         [settings [(str "Invalid settings file (must be JSON): " pathstr)]]
         (try
           (settings-from-json settings long-arg-map (slurp pathstr))
           (catch Exception e
             [settings [(str "Unable to parse JSON in settings file: " pathstr)]])))))))

(defn rec-get-settings-from-args ^SearchSettings [^SearchSettings settings long-arg-map args errs]
  (if (or (empty? args) (not (empty? errs)))
    [settings errs]
    (let [arg (first args)
          a (if (.startsWith arg "-") (str/replace arg #"^\-+" ""))
          k (if a (get long-arg-map a))
          a2 (second args)]
      (if a
        (cond
          ;; 1) no k
          (not k)
          (rec-get-settings-from-args settings long-arg-map (rest args) (conj errs (str "Invalid option: " a)))

          ;; 2) boolean option
          (contains? bool-action-map k)
          (rec-get-settings-from-args ((k bool-action-map) settings true) long-arg-map (rest args) errs)

          ;; 3) option without arg
          (not a2)
          (rec-get-settings-from-args settings long-arg-map (rest args) (conj errs (str "Missing arg for option " a)))

          ;; 4) string option
          (contains? string-action-map k)
          (rec-get-settings-from-args ((k string-action-map) settings a2) long-arg-map (drop 2 args) errs)

          ;; 5) int option
          (contains? int-action-map k)
          (rec-get-settings-from-args ((k int-action-map) settings a2) long-arg-map (drop 2 args) errs)

          ;; 6) long option
          (contains? long-action-map k)
          (rec-get-settings-from-args ((k long-action-map) settings a2) long-arg-map (drop 2 args) errs)

          ;; 7) settings-file option
          (= k :settings-file)
          (let [[file-settings file-errs] (settings-from-file settings long-arg-map a2)]
            (rec-get-settings-from-args file-settings long-arg-map (drop 2 args) (concat errs file-errs)))

          :else
          (rec-get-settings-from-args settings long-arg-map (rest args) (conj errs (str "Invalid option: " a))))
        (rec-get-settings-from-args (add-path settings (to-path arg)) long-arg-map (rest args) errs)))))

(defn settings-from-args ^SearchSettings [args]
  ;; default print-results to true since running as cli
  (let [initial-settings (assoc DEFAULT-SEARCH-SETTINGS :print-results true)
        long-arg-map (get-long-arg-map)]
    (rec-get-settings-from-args initial-settings long-arg-map args [])))

(defn longest-length [options]
  (let [lens (map #(+ (count (:long-arg %)) (if (:short-arg %) 3 0)) options)]
    (apply max lens)))

(defn option-to-string ^String [^SearchOption opt longest]
  (let [s (:short-arg opt)
        l (:long-arg opt)
        d (:desc opt)
        short-string (if (not (empty? s)) (str "-" s ",") "")
        opt-string (str short-string "--" l)]
    (format (str "%-" longest "s %s") opt-string d)))

(defn usage-string []
  (let [longest (longest-length SEARCH-OPTIONS)]
    (str
     "Usage:\n"
     " cljsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
     "Options:\n "
     (str/join "\n " (map #(option-to-string % longest) SEARCH-OPTIONS)))))

(defn usage
  ([exit-code]
    (log-msg "" (usage-string) "")
    (System/exit exit-code))
  ([]
    (usage 0)))
