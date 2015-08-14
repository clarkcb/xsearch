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
  (:require [clojure.string :as str])
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (lower-case)]
        [clojure.xml :only (parse)]
        [cljsearch.common :only (log-msg)]
        [cljsearch.config :only (SEARCHOPTIONSPATH)]
        [cljsearch.fileutil :only (expand-path)]
        [cljsearch.searchsettings :only
          (->SearchSettings DEFAULT-SETTINGS add-extension add-pattern
            set-archivesonly set-debug)]))

(defrecord SearchOption [short-arg long-arg desc])

(defn get-sortarg [so]
  (if (= "" (:short-arg so))
    (:long-arg so)
    (str (str/lower-case (:short-arg so)) "a" (:long-arg so))))

(defn get-searchoptions-from-xml [f]
  (let [sofile (File. (expand-path SEARCHOPTIONSPATH))
        searchoptions (filter #(= :searchoption (:tag %)) (xml-seq (parse sofile)))
        longnames (map :long (map :attrs searchoptions))
        shortnames (map :short (map :attrs searchoptions))
        longshortmap (zipmap longnames shortnames)
        descs (map #(.trim (first %)) (map :content searchoptions))
        longdescmap (zipmap longnames descs)
        get-short (fn [l] (get longshortmap l))
        get-desc (fn [l] (get longdescmap l))]
    (sort-by get-sortarg (map #(SearchOption. (get-short %) % (get-desc %)) longnames))))

(def OPTIONS (get-searchoptions-from-xml SEARCHOPTIONSPATH))

(defn print-option [opt]
  (let [format-string "(SearchOption short=\"%s\" long=\"%s\" desc=\"%s\")"]
    (println
      (format format-string (:short-arg opt) (:long-arg opt) (:desc opt)))))

(defn print-options []
  (doseq [o OPTIONS] (print-option o)))

(def arg-action-map
  { :in-archiveext (fn [settings s] (add-extension settings s :in-archiveextensions))
    :in-archivefilepattern (fn [settings s] (add-pattern settings s :in-archivefilepatterns))
    :in-dirpattern (fn [settings s] (add-pattern settings s :in-dirpatterns))
    :in-ext (fn [settings s] (add-extension settings s :in-extensions))
    :in-filepattern (fn [settings s] (add-pattern settings s :in-filepatterns))
    :in-linesafterpattern (fn [settings s] (add-pattern settings s :in-linesafterpatterns))
    :in-linesbeforepattern (fn [settings s] (add-pattern settings s :in-linesbeforepatterns))
    :linesafter (fn [settings s] (assoc settings :linesafter (read-string s)))
    :linesaftertopattern (fn [settings s] (add-pattern settings s :linesaftertopatterns))
    :linesafteruntilpattern (fn [settings s] (add-pattern settings s :linesafteruntilpatterns))
    :linesbefore (fn [settings s] (assoc settings :linesbefore (read-string s)))
    :maxlinelength (fn [settings s] (assoc settings :maxlinelength (read-string s)))
    :out-archiveext (fn [settings s] (add-extension settings s :out-archiveextensions))
    :out-archivefilepattern (fn [settings s] (add-pattern settings s :out-archivefilepattern))
    :out-dirpattern (fn [settings s] (add-pattern settings s :out-dirpatterns))
    :out-ext (fn [settings s]  (add-extension settings s :out-extensions))
    :out-filepattern (fn [settings s] (add-pattern settings s :out-filepatterns))
    :out-linesafterpattern (fn [settings s] (add-pattern settings s :out-linesafterpatterns))
    :out-linesbeforepattern (fn [settings s] (add-pattern settings s :out-linesbeforepatterns))
    :search (fn [settings s] (add-pattern settings s :searchpatterns))
  })

(def flag-action-map
  { :allmatches (fn [settings] (assoc settings :firstmatch false))
    :archivesonly (fn [settings] (set-archivesonly settings))
    :debug (fn [settings] (set-debug settings))
    :excludehidden (fn [settings] (assoc settings :excludehidden true))
    :firstmatch (fn [settings] (assoc settings :firstmatch true))
    :help (fn [settings] (assoc settings :printusage true))
    :includehidden (fn [settings] (assoc settings :excludehidden false))
    :listdirs (fn [settings] (assoc settings :listdirs true))
    :listfiles (fn [settings] (assoc settings :listfiles true))
    :listlines (fn [settings] (assoc settings :listlines true))
    :multilinesearch (fn [settings] (assoc settings :multilinesearch true))
    :noprintmatches (fn [settings] (assoc settings :printresults false))
    :norecursive (fn [settings] (assoc settings :recursive false))
    :nosearcharchives (fn [settings] (assoc settings :searcharchives false))
    :printmatches (fn [settings] (assoc settings :printresults true))
    :recursive (fn [settings] (assoc settings :recursive true))
    :searcharchives (fn [settings] (assoc settings :searcharchives true))
    :uniquelines (fn [settings] (assoc settings :uniquelines true))
    :verbose (fn [settings] (assoc settings :verbose true))
    :version (fn [settings] (assoc settings :version true))
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

;(println (str "firstmatch: " (get-long-arg "firstmatch") "\n"))
;(println (str "1: " (get-long-arg "1") "\n"))
;(println (str "blarg: " (get-long-arg "blarg") "\n"))

(defn settings-from-args
  ([args]
    (settings-from-args DEFAULT-SETTINGS args []))
  ([settings args errs]
    (if (empty? args)
      [settings errs]
      (let [arg (first args)
            a (if (.startsWith arg "-") (str/replace arg #"^\-+" ""))
            k (if a (get-long-arg a))
            a2 (second args)]
        (if a
          (do
            (cond
              (contains? arg-action-map k)
                (if a2
                  (settings-from-args ((k arg-action-map) settings a2) (rest (rest args)) errs)
                  (settings-from-args settings (rest args) (conj errs (str "Missing arg for option " a))))
              (contains? flag-action-map k)
                (settings-from-args ((k flag-action-map) settings) (rest args) errs)
              :else
                (settings-from-args settings (rest args) (conj errs (str "Invalid option: " a)))))
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
