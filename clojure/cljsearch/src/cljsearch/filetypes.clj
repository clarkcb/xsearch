;;; ############################################################################
;;;
;;; fileutil.clj
;;;
;;; Utility functions for getting file extension and determining file type
;;;
;;; ############################################################################

(ns cljsearch.filetypes
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:use [clojure.set :only (union)]
        [clojure.string :only (split)]
        [clojure.xml :only (parse)]
        [cljsearch.config :only (FILETYPESPATH)]
        [cljsearch.fileutil :only (expand-path get-ext)]))

(defn get-filetypemap [f]
  (let [ftfile (File. (expand-path FILETYPESPATH))
        filetypes (filter #(= :filetype (:tag %)) (xml-seq (parse ftfile)))
        typenames (map :name (map :attrs filetypes))
        extension-nodes (map first (map :content (map first (map :content filetypes))))
        extension-sets (map #(set %) (map #(split % #"\s+") extension-nodes))
        filetypemap (zipmap typenames extension-sets)
        textmap (hash-map "text"
                  (union (get filetypemap "text")
                         (get filetypemap "code")
                         (get filetypemap "xml")))
        searchablemap (hash-map "searchable"
                        (union (get filetypemap "archive")
                               (get filetypemap "binary")
                               (get filetypemap "text")))
        ]
  (merge filetypemap textmap searchablemap)))

(def FILETYPEMAP (get-filetypemap FILETYPESPATH))

(defn binary-file? [f]
  (contains? (get FILETYPEMAP "binary") (get-ext f)))

(defn archive-file? [f]
  (contains? (get FILETYPEMAP "archive") (get-ext f)))

(defn searchable-file? [f]
  (contains? (get FILETYPEMAP "searchable") (get-ext f)))

(defn text-file? [f]
  (contains? (get FILETYPEMAP "text") (get-ext f)))

(defn get-filetype [f]
  (cond
    (text-file? f) :text
    (binary-file? f) :binary
    (archive-file? f) :archive
    :else :unknown))
