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
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.fileutil :only (expand-path)]
        [cljfind.findsettings :only (sort-by-from-name)]
        [cljsearch.searchsettings :only
         (->SearchSettings DEFAULT-SETTINGS add-extension add-file-type add-pattern
            add-path set-archives-only set-debug set-num)]))

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
  { :encoding (fn [settings s] (assoc settings :text-file-encoding s))
    :in-archiveext (fn [settings s] (add-extension settings s :in-archive-extensions))
    :in-archivefilepattern (fn [settings s] (add-pattern settings s :in-archive-file-patterns))
    :in-dirpattern (fn [settings s] (add-pattern settings s :in-dir-patterns))
    :in-ext (fn [settings s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [settings s] (add-pattern settings s :in-file-patterns))
    :in-filetype (fn [settings s] (add-file-type settings s :in-file-types))
    :in-linesafterpattern (fn [settings s] (add-pattern settings s :in-lines-after-patterns))
    :in-linesbeforepattern (fn [settings s] (add-pattern settings s :in-lines-before-patterns))
    :linesafter (fn [settings s] (set-num settings s :lines-after))
    :linesaftertopattern (fn [settings s] (add-pattern settings s :lines-after-to-patterns))
    :linesafteruntilpattern (fn [settings s] (add-pattern settings s :lines-after-until-patterns))
    :linesbefore (fn [settings s] (set-num settings s :lines-before))
    :maxdepth (fn [settings s] (assoc settings :max-depth (Integer/parseInt s)))
    :maxlastmod (fn [settings s] (assoc settings :max-last-mod (clojure.instant/read-instant-date s)))
    :maxlinelength (fn [settings s] (assoc settings :max-line-length (read-string s)))
    :maxsize (fn [settings s] (assoc settings :max-size (Integer/parseInt s)))
    :mindepth (fn [settings s] (assoc settings :min-depth (Integer/parseInt s)))
    :minlastmod (fn [settings s] (assoc settings :min-last-mod (clojure.instant/read-instant-date s)))
    :minsize (fn [settings s] (assoc settings :min-size (Integer/parseInt s)))
    :out-archiveext (fn [settings s] (add-extension settings s :out-archive-extensions))
    :out-archivefilepattern (fn [settings s] (add-pattern settings s :out-archive-file-patterns))
    :out-dirpattern (fn [settings s] (add-pattern settings s :out-dir-patterns))
    :out-ext (fn [settings s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [settings s] (add-pattern settings s :out-file-patterns))
    :out-filetype (fn [settings s] (add-file-type settings s :out-file-types))
    :out-linesafterpattern (fn [settings s] (add-pattern settings s :out-lines-after-patterns))
    :out-linesbeforepattern (fn [settings s] (add-pattern settings s :out-lines-before-patterns))
    :path (fn [settings s] (add-path settings s))
    :searchpattern (fn [settings s] (add-pattern settings s :search-patterns))
    :sort-by (fn [settings s] (assoc settings :sort-by (sort-by-from-name s)))
  })

(def bool-flag-action-map
  { :allmatches (fn [settings b] (assoc settings :first-match (not b)))
    :archivesonly (fn [settings b] (set-archives-only settings b))
    :colorize (fn [settings b] (assoc settings :colorize b))
    :debug (fn [settings b] (set-debug settings b))
    :excludehidden (fn [settings b] (assoc settings :exclude-hidden b))
    :firstmatch (fn [settings b] (assoc settings :first-match b))
    :help (fn [settings b] (assoc settings :print-usage b))
    :includehidden (fn [settings b] (assoc settings :exclude-hidden (not b)))
    :listdirs (fn [settings b] (assoc settings :list-dirs b))
    :listfiles (fn [settings b] (assoc settings :list-files b))
    :listlines (fn [settings b] (assoc settings :list-lines b))
    :multilinesearch (fn [settings b] (assoc settings :multi-line-search b))
    :nocolorize (fn [settings b] (assoc settings :colorize (not b)))
    :noprintmatches (fn [settings b] (assoc settings :print-results (not b)))
    :norecursive (fn [settings b] (assoc settings :recursive (not b)))
    :nosearcharchives (fn [settings b] (assoc settings :search-archives (not b)))
    :printmatches (fn [settings b] (assoc settings :print-results b))
    :recursive (fn [settings b] (assoc settings :recursive b))
    :searcharchives (fn [settings b] (assoc settings :search-archives b))
    :sort-ascending (fn [settings b] (assoc settings :sort-descending (not b)))
    :sort-caseinsensitive (fn [settings b] (assoc settings :sort-case-insensitive b))
    :sort-casesensitive (fn [settings b] (assoc settings :sort-case-insensitive (not b)))
    :sort-descending (fn [settings b] (assoc settings :sort-descending b))
    :uniquelines (fn [settings b] (assoc settings :unique-lines b))
    :verbose (fn [settings b] (assoc settings :verbose b))
    :version (fn [settings b] (assoc settings :version b))
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

(defn settings-from-map [settings ks m errs]
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
  ([json]
    (settings-from-json DEFAULT-SETTINGS json))
  ([settings json]
    (let [obj (json/read-str json :key-fn keyword)
          ks (keys obj)]
      (settings-from-map settings ks obj []))))

(defn settings-from-file [settings f]
  (let [contents (slurp f)]
    (settings-from-json settings contents)))

(defn settings-from-args
  ([args]
    (settings-from-args DEFAULT-SETTINGS args []))
  ([settings args errs]
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
          (settings-from-args (add-path settings arg) (rest args) errs))))))

(defn longest-length [options]
  (let [lens (map #(+ (count (:long-arg %)) (if (:short-arg %) 3 0)) options)]
    (apply max lens)))

(defn option-to-string [opt longest]
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
