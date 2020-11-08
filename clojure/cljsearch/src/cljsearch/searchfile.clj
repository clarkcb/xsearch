;;; ############################################################################
;;;
;;; searchfile.clj
;;;
;;; Encapsulates a file to be searched
;;;
;;; ############################################################################

(ns cljsearch.searchfile
  #^{:author "Cary Clark",
     :doc "Encapsulates a file to be searched"}
  (:use [clojure.string :as str :only (join trim trim-newline)]))

; record to hold a search-file (file is a File object)
(defrecord SearchFile [containers file filetype])

(defn new-search-file
  ([^java.io.File file filetype]
   (new-search-file [] file filetype))
  ([containers file filetype]
   (->SearchFile containers file filetype)))

(defn search-file-path [^SearchFile sf]
  (str (if (empty? (:containers sf)) "" (str (str/join "!" (:containers sf)) "!"))
       (.getPath (:file sf))))
