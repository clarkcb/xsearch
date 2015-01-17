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
  (:import (java.io File))
  (use [clojure.set :only (union)])
  (use [clojure.string :only (join split)])
  (use [clojure.xml]))

(defrecord SearchSettings
  [
    archivesonly
    debug
    dotiming
    excludehidden
    firstmatch
    in-archiveextensions
    in-archivefilepatterns
    in-dirpatterns
    in-extensions
    in-filepatterns
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
    out-linesafterpatterns
    out-linesbeforepatterns
    printresults
    printusage
    printversion
    recursive
    searcharchives
    searchpatterns
    startpath
    uniquelines
    verbose
  ])

(def DEFAULT-SETTINGS (->SearchSettings
    false ; archivesonly
    false ; debug
    false ; dotiming
    true  ; excludehidden
    false ; firstmatch
    []    ; in-archiveextensions
    []    ; in-archivefilepatterns
    []    ; in-dirpatterns
    []    ; in-extensions
    []    ; in-filepatterns
    []    ; in-linesafterpatterns
    []    ; in-linesbeforepatterns
    0     ; linesafter
    []    ; linesaftertopatterns
    []    ; linesafteruntilpatterns
    0     ; linesbefore
    false ; listdirs
    false ; listfiles
    false ; listlines
    150   ; maxlinelength
    false ; multilinesearch
    []    ; out-archiveextensions
    []    ; out-archivefilepatterns
    []    ; out-dirpatterns
    []    ; out-extensions
    []    ; out-filepatterns
    []    ; out-linesafterpatterns
    []    ; out-linesbeforepatterns
    true  ; printresults
    false ; printusage
    false ; printversion
    true  ; recursive
    false ; searcharchives
    []    ; searchpatterns
    ""    ; startpath
    false ; uniquelines
    false ; verbose
  ))

(defn add-element [x coll]
  (conj coll x))

(defn add-extension [settings x extname]
  (update-in settings [extname] #(add-element x %)))

(defn add-pattern [settings p patname]
  (update-in settings [patname] #(add-element p %)))
