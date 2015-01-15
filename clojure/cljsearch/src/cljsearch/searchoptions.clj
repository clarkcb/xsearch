;;; ############################################################################
;;;
;;; searchoptions.clj
;;;
;;; Defines the available command-line options and utility functions
;;;
;;; ############################################################################

(ns cljsearch
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (use [clojure.set :only (union)])
  (use [clojure.string :only (join split)])
  (use [clojure.xml]))

(def OPTIONS
  [{:short "1", :long "firstmatch", :arg false, :desc "show only the first match for a file+search combination"}
   {:short "a", :long "allmatches", :arg false, :desc "show all matches"}
   {:short "b", :long "search-binary", :arg false, :desc "include binary files in search"}
   {:short "B", :long "no-search-binary", :arg false, :desc "do NOT include binary files in search"}
   {            :long "filelist", :arg false, :desc "print the list of unique files with matches"}
   {:short "f", :long "filenamepattern", :arg true, :desc "regex pattern for file name to search"}
   {:short "F", :long "no-filenamepattern", :arg true, :desc "regex pattern for file name NOT to search"}
   {            :long "linelist", :arg false, :desc "print the list of unique lines with matches"}
   {:short "p", :long "printlines", :arg false, :desc "print the lines as they are found"}
   {:short "P", :long "no-printlines", :arg false, :desc "do not print lines as they are found"}
   {:short "s", :long "searchstring", :arg true, :desc "a search string (regex pattern)"}
   {:short "x", :long "ext", :arg true, :desc "a file extension of files to search"}
   {:short "X", :long "no-ext", :arg true, :desc "a file extension of files NOT to search"}
   {:short "z", :long "search-compressed", :arg false, :desc "include compressed files in search"}
   {:short "Z", :long "no-search-compressed", :arg false, :desc "do NOT include compressed files in search"}
  ])

; struct to hold a search-result
(defstruct search-result :file :line :linenum :pattern)

; ref to contain the list of search-result structs
(def search-results (ref []))

(struct search-result file line linenum pattern)
