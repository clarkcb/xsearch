;;; ############################################################################
;;;
;;; file-types.clj
;;;
;;; Utility functions for getting file extension and determining file type
;;;
;;; ############################################################################

(ns cljsearch.filetypes
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :only (split lower-case)]
        [clojure.xml :only (parse)]
        [cljsearch.fileutil :only (expand-path get-ext)]))

(def ARCHIVE "archive")
(def BINARY "binary")
(def CODE "code")
(def TEXT "text")
(def XML "xml")

(defn get-file-type-map-from-json []
  (let [contents (slurp (io/resource "filetypes.json"))
        file-types-objs (:filetypes (json/read-str contents :key-fn keyword))
        typenames (map :type file-types-objs)
        extension-sets (map #(set %) (map :extensions file-types-objs))
        file-type-map (zipmap typenames extension-sets)
        text-map (hash-map "all-text"
                   (union (get file-type-map TEXT)
                          (get file-type-map CODE)
                          (get file-type-map XML)))
        searchable-map (hash-map "searchable"
                         (union (get file-type-map ARCHIVE)
                                (get file-type-map BINARY)
                                (get file-type-map TEXT)))
        fullmap (merge file-type-map text-map searchable-map)
        ]
    fullmap))

(def FILETYPEMAP (get-file-type-map-from-json))

(defn archive-ext? [ext]
  (contains? (get FILETYPEMAP ARCHIVE) ext))

(defn archive-file? [f]
  (archive-ext? (get-ext f)))

(defn binary-ext? [ext]
  (contains? (get FILETYPEMAP BINARY) ext))

(defn binary-file? [f]
  (contains? (get FILETYPEMAP BINARY) (get-ext f)))

(defn code-ext? [ext]
  (contains? (get FILETYPEMAP CODE) ext))

(defn code-file? [f]
  (contains? (get FILETYPEMAP CODE) (get-ext f)))

(defn searchable-file? [f]
  (contains? (get FILETYPEMAP "searchable") (get-ext f)))

(defn text-ext? [ext]
  (contains? (get FILETYPEMAP TEXT) ext))

(defn text-file? [f]
  (contains? (get FILETYPEMAP TEXT) (get-ext f)))

(defn xml-ext? [ext]
  (contains? (get FILETYPEMAP XML) ext))

(defn xml-file? [f]
  (contains? (get FILETYPEMAP XML) (get-ext f)))

(defn get-file-type [f]
  (let [ext (get-ext f)]
    (cond
      (text-ext? ext) :text
      (binary-ext? ext) :binary
      (archive-ext? ext) :archive
      (code-ext? ext) :code
      (xml-ext? ext) :xml
      :else :unknown)))

(defn unknown-file? [f]
  (= :unknown (get-file-type f)))

(defn from-name [name]
  (cond
    (= TEXT (lower-case name)) :text
    (= BINARY (lower-case name)) :binary
    (= ARCHIVE (lower-case name)) :archive
    (= CODE (lower-case name)) :code
    (= XML (lower-case name)) :xml
    :else :unknown))
