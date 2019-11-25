;;; ############################################################################
;;;
;;; searchsettings.clj
;;;
;;; Defines the settings for a given search instance
;;;
;;; ############################################################################

(ns cljsearch.searchsettings
  #^{:author "Cary Clark",
     :doc "Defines the settings for a given search instance"}
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (split)]
        [cljsearch.filetypes :only (from-name)]))

(defrecord SearchSettings
  [
    archivesonly
    debug
    excludehidden
    firstmatch
    in-archiveextensions
    in-archivefilepatterns
    in-dirpatterns
    in-extensions
    in-filepatterns
    in-filetypes
    in-linesafterpatterns
    in-linesbeforepatterns
    linesafter
    linesaftertopatterns
    linesafteruntilpatterns
    linesbefore
    listdirs
    listfiles
    listlines
    maxlinelength
    multilinesearch
    out-archiveextensions
    out-archivefilepatterns
    out-dirpatterns
    out-extensions
    out-filepatterns
    out-filetypes
    out-linesafterpatterns
    out-linesbeforepatterns
    printresults
    printusage
    printversion
    recursive
    searcharchives
    searchpatterns
    startpath
    textfileencoding
    uniquelines
    verbose
  ])

(def DEFAULT-SETTINGS (->SearchSettings
    false   ; archivesonly
    false   ; debug
    true    ; excludehidden
    false   ; firstmatch
    #{}     ; in-archiveextensions
    #{}     ; in-archivefilepatterns
    #{}     ; in-dirpatterns
    #{}     ; in-extensions
    #{}     ; in-filepatterns
    #{}     ; in-filetypes
    #{}     ; in-linesafterpatterns
    #{}     ; in-linesbeforepatterns
    0       ; linesafter
    #{}     ; linesaftertopatterns
    #{}     ; linesafteruntilpatterns
    0       ; linesbefore
    false   ; listdirs
    false   ; listfiles
    false   ; listlines
    150     ; maxlinelength
    false   ; multilinesearch
    #{}     ; out-archiveextensions
    #{}     ; out-archivefilepatterns
    #{}     ; out-dirpatterns
    #{}     ; out-extensions
    #{}     ; out-filepatterns
    #{}     ; out-filetypes
    #{}     ; out-linesafterpatterns
    #{}     ; out-linesbeforepatterns
    true    ; printresults
    false   ; printusage
    false   ; printversion
    true    ; recursive
    false   ; searcharchives
    #{}     ; searchpatterns
    nil     ; startpath
    "utf-8" ; textfileencoding
    false   ; uniquelines
    false   ; verbose
  ))

(defn add-element [x coll]
  (conj coll x))

(defn add-extensions [settings exts extname]
  (if (empty? exts)
    settings
    (add-extensions
      (update-in settings [extname] #(add-element (first exts) %)) (rest exts) extname)))

(defn add-extension [settings ext extname]
  (let [t (type ext)]
    (cond
      (= t (type []))
        (add-extensions settings ext extname)
      :else
        (add-extensions settings (str/split ext #",") extname))))

(defn add-filetypes [settings types typesname]
  (if (empty? types)
    settings
    (add-filetypes
     (update-in settings [typesname] #(add-element (from-name (first types)) %)) (rest types) typesname)))

(defn add-filetype [settings typ typesname]
  (let [t (type typ)]
    (cond
      (= t (type []))
      (add-filetypes settings typ typesname)
      :else
      (add-filetypes settings (str/split typ #",") typesname))))

(defn add-patterns [settings pats patname]
  (if (empty? pats)
    settings
    (add-patterns
      (update-in settings [patname] #(add-element (re-pattern (first pats)) %)) (rest pats) patname)))

(defn add-pattern [settings p patname]
  (let [t (type p)]
    (cond
      (= t (type []))
        (add-patterns settings p patname)
      :else
        (add-patterns settings [p] patname))))

(defn set-num [settings n numname]
  (let [t (type n)]
    (cond
      (= t java.lang.Long)
        (assoc settings numname n)
      :else
        (assoc settings numname (read-string n)))))

(defn set-archivesonly [settings b]
  (let [with-searcharchives (assoc settings :searcharchives b)]
    (if b
      (assoc with-searcharchives :archivesonly true)
      with-searcharchives)))

(defn set-debug [settings b]
  (let [with-debug (assoc settings :debug true)]
    (if b
      (assoc with-debug :verbose true)
      with-debug)))
