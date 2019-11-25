;;; ############################################################################
;;;
;;; searchoptions.clj
;;;
;;; Defines the available command-line options and utility functions
;;;
;;; ############################################################################

(ns cljsearch.searchoptions
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [clojure.xml :only (parse)]
        [cljsearch.common :only (log-msg)]
        [cljsearch.fileutil :only (expand-path)]
        [cljsearch.searchsettings :only
         (->SearchSettings DEFAULT-SETTINGS add-extension add-filetype add-pattern
            set-archivesonly set-debug set-num)]))

(defrecord SearchOption [short-arg long-arg desc])

(defn get-sortarg [so]
  (if (= "" (:short-arg so))
    (:long-arg so)
    (str (str/lower-case (:short-arg so)) "a" (:long-arg so))))

(defn get-searchoptions-from-xml []
  (let [socontents (slurp (io/resource "searchoptions.xml"))
        sostream (java.io.ByteArrayInputStream. (.getBytes socontents))
        searchoptions (filter #(= :searchoption (:tag %)) (xml-seq (parse sostream)))
        longnames (map :long (map :attrs searchoptions))
        shortnames (map :short (map :attrs searchoptions))
        longshortmap (zipmap longnames shortnames)
        descs (map #(.trim (first %)) (map :content searchoptions))
        longdescmap (zipmap longnames descs)
        get-short (fn [l] (get longshortmap l))
        get-desc (fn [l] (get longdescmap l))]
    (sort-by get-sortarg (map #(SearchOption. (get-short %) % (get-desc %)) longnames))))

(defn get-searchoptions-from-json []
  (let [contents (slurp (io/resource "searchoptions.json"))
        searchoptions-objs (:searchoptions (json/read-str contents :key-fn keyword))
        longnames (map #(get % :long) searchoptions-objs)
        shortnames (map #(get % :short "") searchoptions-objs)
        longshortmap (zipmap longnames shortnames)
        descs (map #(.trim %) (map :desc searchoptions-objs))
        longdescmap (zipmap longnames descs)
        get-short (fn [l] (get longshortmap l))
        get-desc (fn [l] (get longdescmap l))]
    (sort-by get-sortarg (map #(SearchOption. (get-short %) % (get-desc %)) longnames))))

(def OPTIONS (get-searchoptions-from-json))

(defn print-option [opt]
  (let [format-string "(SearchOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o OPTIONS] (print-option o)))

(def arg-action-map
  { :encoding (fn [settings s] (assoc settings :textfileencoding s))
    :in-archiveext (fn [settings s] (add-extension settings s :in-archiveextensions))
    :in-archivefilepattern (fn [settings s] (add-pattern settings s :in-archivefilepatterns))
    :in-dirpattern (fn [settings s] (add-pattern settings s :in-dirpatterns))
    :in-ext (fn [settings s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [settings s] (add-pattern settings s :in-filepatterns))
    :in-filetype (fn [settings s] (add-filetype settings s :in-filetypes))
    :in-linesafterpattern (fn [settings s] (add-pattern settings s :in-linesafterpatterns))
    :in-linesbeforepattern (fn [settings s] (add-pattern settings s :in-linesbeforepatterns))
    :linesafter (fn [settings s] (set-num settings s :linesafter))
    :linesaftertopattern (fn [settings s] (add-pattern settings s :linesaftertopatterns))
    :linesafteruntilpattern (fn [settings s] (add-pattern settings s :linesafteruntilpatterns))
    :linesbefore (fn [settings s] (set-num settings s :linesbefore))
    :maxlinelength (fn [settings s] (assoc settings :maxlinelength (read-string s)))
    :out-archiveext (fn [settings s] (add-extension settings s :out-archiveextensions))
    :out-archivefilepattern (fn [settings s] (add-pattern settings s :out-archivefilepattern))
    :out-dirpattern (fn [settings s] (add-pattern settings s :out-dirpatterns))
    :out-ext (fn [settings s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [settings s] (add-pattern settings s :out-filepatterns))
    :out-filetype (fn [settings s] (add-filetype settings s :out-filetypes))
    :out-linesafterpattern (fn [settings s] (add-pattern settings s :out-linesafterpatterns))
    :out-linesbeforepattern (fn [settings s] (add-pattern settings s :out-linesbeforepatterns))
    :search (fn [settings s] (add-pattern settings s :searchpatterns))
  })

(def bool-flag-action-map
  { :allmatches (fn [settings b] (assoc settings :firstmatch (not b)))
    :archivesonly (fn [settings b] (set-archivesonly settings b))
    :debug (fn [settings b] (set-debug settings b))
    :excludehidden (fn [settings b] (assoc settings :excludehidden b))
    :firstmatch (fn [settings b] (assoc settings :firstmatch b))
    :help (fn [settings b] (assoc settings :printusage b))
    :includehidden (fn [settings b] (assoc settings :excludehidden (not b)))
    :listdirs (fn [settings b] (assoc settings :listdirs b))
    :listfiles (fn [settings b] (assoc settings :listfiles b))
    :listlines (fn [settings b] (assoc settings :listlines b))
    :multilinesearch (fn [settings b] (assoc settings :multilinesearch b))
    :noprintmatches (fn [settings b] (assoc settings :printresults (not b)))
    :norecursive (fn [settings b] (assoc settings :recursive (not b)))
    :nosearcharchives (fn [settings b] (assoc settings :searcharchives (not b)))
    :printmatches (fn [settings b] (assoc settings :printresults b))
    :recursive (fn [settings b] (assoc settings :recursive b))
    :searcharchives (fn [settings b] (assoc settings :searcharchives b))
    :uniquelines (fn [settings b] (assoc settings :uniquelines b))
    :verbose (fn [settings b] (assoc settings :verbose b))
    :version (fn [settings b] (assoc settings :version b))
  })

(defn get-long-arg [arg]
  (let [longnames (map :long-arg OPTIONS)
        longmap (zipmap longnames (repeat 1))
        shortoptions (remove #(= (:short-arg %) "") OPTIONS)
        shortlongmap (zipmap (map :short-arg shortoptions) (map :long-arg shortoptions))]
    (cond
      (contains? longmap arg) (keyword arg)
      (contains? shortlongmap arg) (keyword (get shortlongmap arg))
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
        (= k :startpath)
          (do
            (settings-from-map (assoc settings :startpath v) (rest ks) m errs))
        :else
          (settings-from-map settings (rest ks) m (conj errs (str "Invalid option: " k)))))))

(defn settings-from-json [settings js]
  (let [obj (json/read-str js :key-fn keyword)
        ks (keys obj)]
    (settings-from-map settings ks obj [])))

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
          (settings-from-args (assoc settings :startpath arg) (rest args) errs))))))

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
      " cljsearch [options] -s <searchpattern> <startpath>\n\n"
      "Options:\n "
      (str/join "\n " (map #(option-to-string % longest) OPTIONS)))))

(defn usage []
  (log-msg "" (usage-string) "")
  (System/exit 0))
