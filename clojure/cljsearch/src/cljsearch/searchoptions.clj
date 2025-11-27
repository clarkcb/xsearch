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
        [cljfind.common :only (as-keyword as-string log-msg)]
        [cljfind.argtokenizer]
        [cljfind.fileutil :only (exists-path? expand-path path-str to-path)]
        [cljfind.findsettings :only
         (add-extension add-file-type add-path add-pattern
          set-archives-only set-debug set-int-val set-long-val
          sort-by-from-name)]
        [cljsearch.searchsettings :only
         (->SearchSettings DEFAULT-SEARCH-SETTINGS)]))

(defrecord SearchOption [short-arg long-arg desc arg-type])

(defn get-sort-arg ^String [^SearchOption so]
  (if (= "" (:short-arg so))
    (:long-arg so)
    (str (str/lower-case (:short-arg so)) "a" (:long-arg so))))

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

(defn get-action-arg-type [^String long-arg]
  (let [long-key (as-keyword long-arg)]
    (cond
      (contains? (set (keys bool-action-map)) long-key) :bool
      (contains? (set (keys string-action-map)) long-key) :string
      (contains? (set (keys int-action-map)) long-key) :int
      (contains? (set (keys long-action-map)) long-key) :long
      (= long-key :path) :string
      (= long-key :settings-file) :string
      :else :unknown)))

(defn get-search-options-from-json []
  (let [contents (slurp (io/resource "searchoptions.json"))
        search-options-objs (:searchoptions (json/read-str contents :key-fn keyword))]
    (map #(->SearchOption (get % :short "") (get % :long) (get % :desc) (get-action-arg-type (get % :long))) search-options-objs)))

(def ^:const SEARCH-OPTIONS (get-search-options-from-json))

(def arg-tokenizer (get-arg-tokenizer-for-options SEARCH-OPTIONS))

(defn print-option [^SearchOption opt]
  (let [format-string "(SearchOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
     (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o SEARCH-OPTIONS] (print-option o)))

(defn get-long-arg-map []
  (let [long-names     (concat (map :long-arg SEARCH-OPTIONS) ["path"])
        long-map       (zipmap long-names (map #(keyword %) long-names))
        short-options  (remove #(= (:short-arg %) "") SEARCH-OPTIONS)
        short-long-map (zipmap (map :short-arg short-options) (map #(keyword %) (map :long-arg short-options)))]
    (merge long-map short-long-map)))

(declare update-settings-from-file)

(defn update-settings-from-tokens
  ([^SearchSettings settings tokens]
   (update-settings-from-tokens settings tokens []))
  ([^SearchSettings settings tokens errs]
   (if (or (empty? tokens) (not (empty? errs)))
     [settings errs]
     (let [token (first tokens)
           name (:name token)
           arg-type (:type token)
           value (:value token)]
       (case arg-type
         :bool
         (cond
           (not (contains? bool-action-map name))
           (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
           (not (boolean? value))
           (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
           :else
           (update-settings-from-tokens ((name bool-action-map) settings value) (rest tokens) errs))
         :string
         (cond
           (and (not (contains? string-action-map name)) (not (= name :settings-file)))
           (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
           (not (string? value))
           (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
           :else
           (if (= name :settings-file)
             (let [[new-settings new-errs] (update-settings-from-file settings value)]
               (if (empty? new-errs)
                 (update-settings-from-tokens new-settings (rest tokens) errs)
                 (update-settings-from-tokens settings (rest tokens) (concat errs new-errs))))
             (update-settings-from-tokens ((name string-action-map) settings value) (rest tokens) errs)))
         :int
         (cond
           (not (contains? int-action-map name))
           (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
           (not (int? value))
           (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
           :else
           (update-settings-from-tokens ((name int-action-map) settings value) (rest tokens) errs))
         :long
         (cond
           (not (contains? long-action-map name))
           (update-settings-from-tokens settings [] [(str "Invalid option: " name)])
           (not (integer? value))
           (update-settings-from-tokens settings [] [(str "Invalid value option: " name)])
           :else
           (update-settings-from-tokens ((name long-action-map) settings value) (rest tokens) errs))
         (update-settings-from-tokens settings [] [(str "Unknown token type: " name arg-type)]))))))

(defn settings-from-tokens [tokens]
  (update-settings-from-tokens DEFAULT-SEARCH-SETTINGS tokens))

(defn update-settings-from-arg-map [^SearchSettings settings arg-map]
  (let [[tokens errs] (tokenize-arg-map arg-tokenizer arg-map)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-arg-map [arg-map]
  (update-settings-from-arg-map DEFAULT-SEARCH-SETTINGS (get-long-arg-map) arg-map))

(defn update-settings-from-json [^SearchSettings settings ^String json]
  (let [[tokens errs] (tokenize-json arg-tokenizer json)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-json [^String json]
  (update-settings-from-json DEFAULT-SEARCH-SETTINGS json))

(defn update-settings-from-file [^SearchSettings settings f]
  (let [[tokens errs] (tokenize-file arg-tokenizer f)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-file [f]
  (update-settings-from-file DEFAULT-SEARCH-SETTINGS (get-long-arg-map) f))

(defn update-settings-from-args [^SearchSettings settings args]
  (let [[tokens errs] (tokenize-args arg-tokenizer args)]
    (if (not (empty? errs))
      [settings errs]
      (update-settings-from-tokens settings tokens))))

(defn settings-from-args [args]
  ;; default print-results to true since running as cli
  (update-settings-from-args (assoc DEFAULT-SEARCH-SETTINGS :print-results true) args))

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
  (let [options (sort-by get-sort-arg SEARCH-OPTIONS)
        longest (longest-length SEARCH-OPTIONS)]
    (str
     "Usage:\n"
     " cljsearch [options] -s <searchpattern> <path> [<path> ...]\n\n"
     "Options:\n "
     (str/join "\n " (map #(option-to-string % longest) options)))))

(defn usage
  ([exit-code]
    (log-msg "" (usage-string) "")
    (System/exit exit-code))
  ([]
    (usage 0)))
