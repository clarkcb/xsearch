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
        [cljfind.fileutil :only (expand-path to-path)]
        [cljfind.findsettings :only (add-extension add-file-type add-path add-pattern set-debug
                                     sort-by-from-name)]
        [cljsearch.searchsettings :only
         (->SearchSettings DEFAULT-SETTINGS set-archives-only)]))

(defrecord SearchOption [short-arg long-arg desc])

(defn get-sort-arg [^SearchOption so]
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

(def OPTIONS (get-search-options-from-json))

(defn print-option [opt]
  (let [format-string "(SearchOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o OPTIONS] (print-option o)))

(def arg-action-map
  { :encoding (fn [^SearchSettings settings ^String s] (assoc settings :text-file-encoding s))
    :in-archiveext (fn [^SearchSettings settings ^String s] (add-extension settings s :in-archive-extensions))
    :in-archivefilepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-archive-file-patterns))
    :in-dirpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-dir-patterns))
    :in-ext (fn [^SearchSettings settings ^String s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-file-patterns))
    :in-filetype (fn [^SearchSettings settings ^String s] (add-file-type settings s :in-file-types))
    :in-linesafterpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-lines-after-patterns))
    :in-linesbeforepattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :in-lines-before-patterns))
    :linesafter (fn [^SearchSettings settings ^String s] (assoc settings :lines-after (Integer/parseInt s)))
    :linesaftertopattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :lines-after-to-patterns))
    :linesafteruntilpattern (fn [^SearchSettings settings ^String s] (add-pattern settings s :lines-after-until-patterns))
    :linesbefore (fn [^SearchSettings settings ^String s] (assoc settings :lines-before (Integer/parseInt s)))
    :maxdepth (fn [^SearchSettings settings ^String s] (assoc settings :max-depth (Integer/parseInt s)))
    :maxlastmod (fn [^SearchSettings settings ^String s] (assoc settings :max-last-mod (clojure.instant/read-instant-date s)))
    :maxlinelength (fn [^SearchSettings settings ^String s] (assoc settings :max-line-length (read-string s)))
    :maxsize (fn [^SearchSettings settings ^String s] (assoc settings :max-size (Integer/parseInt s)))
    :mindepth (fn [^SearchSettings settings ^String s] (assoc settings :min-depth (Integer/parseInt s)))
    :minlastmod (fn [^SearchSettings settings ^String s] (assoc settings :min-last-mod (clojure.instant/read-instant-date s)))
    :minsize (fn [^SearchSettings settings ^String s] (assoc settings :min-size (Integer/parseInt s)))
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

(def bool-flag-action-map
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

(defn get-long-arg [arg]
  (let [long-names (map :long-arg OPTIONS)
        long-map (zipmap long-names (repeat 1))
        short-options (remove #(= (:short-arg %) "") OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map :long-arg short-options))]
    (cond
      (contains? long-map arg) (keyword arg)
      (contains? short-long-map arg) (keyword (get short-long-map arg))
      :else nil)))

(defn settings-from-map ^SearchSettings [^SearchSettings settings ks m errs]
  (if (empty? ks)
    [settings errs]
    (let [k (keyword (first ks))
          v (k m)]
      (cond
        (contains? arg-action-map k)
          (settings-from-map ((k arg-action-map) settings v) (rest ks) m errs)
        (contains? bool-flag-action-map k)
          (settings-from-map ((k bool-flag-action-map) settings v) (rest ks) m errs)
        (= k :path)
          (do
            (settings-from-map (assoc settings :startpath v) (rest ks) m errs))
        :else
          (settings-from-map settings (rest ks) m (conj errs (str "Invalid option: " k)))))))

(defn settings-from-json
  (^SearchSettings [json]
    (settings-from-json DEFAULT-SETTINGS json))
  (^SearchSettings [^SearchSettings settings ^String json]
    (let [obj (json/read-str json :key-fn keyword)
          ks (keys obj)]
      (settings-from-map settings ks obj []))))

(defn settings-from-file ^SearchSettings [^SearchSettings settings f]
  (let [contents (slurp f)]
    (settings-from-json settings contents)))

(defn settings-from-args
  (^SearchSettings [args]
    (settings-from-args DEFAULT-SETTINGS args []))
  (^SearchSettings [^SearchSettings settings args errs]
    (if (or (empty? args) (not (empty? errs)))
      [settings errs]
      (let [arg (first args)
            a (if (.startsWith arg "-") (str/replace arg #"^\-+" ""))
            k (if a (get-long-arg a))
            a2 (second args)]
        (if a
          (cond
            (contains? arg-action-map k)
              (if a2
                (settings-from-args ((k arg-action-map) settings a2) (drop 2 args) errs)
                (settings-from-args settings (rest args) (conj errs (str "Missing arg for option " a))))
            (contains? bool-flag-action-map k)
              (settings-from-args ((k bool-flag-action-map) settings true) (rest args) errs)
            (= k :settings-file)
              (let [[file-settings file-errs] (settings-from-file settings a2)]
                (settings-from-args file-settings (drop 2 args) (concat errs file-errs)))
            :else
              (settings-from-args settings (rest args) (conj errs (str "Invalid option: " a))))
          ;; (settings-from-args (assoc settings :startpath arg) (rest args) errs)
          (settings-from-args (add-path settings (to-path arg)) (rest args) errs))))))

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
  (let [longest (longest-length OPTIONS)]
    (str
      "Usage:\n"
      " cljsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
      "Options:\n "
      (str/join "\n " (map #(option-to-string % longest) OPTIONS)))))

(defn usage
  ([exit-code]
    (log-msg "" (usage-string) "")
    (System/exit exit-code))
  ([]
    (usage 0)))
