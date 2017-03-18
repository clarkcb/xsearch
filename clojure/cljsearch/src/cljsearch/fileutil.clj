;;; ############################################################################
;;;
;;; fileutil.clj
;;;
;;; File-related utility functions
;;;
;;; ############################################################################

(ns cljsearch.fileutil
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:use [clojure.string :only (split)]
        [cljsearch.searchfile :only (new-search-file)]))

; needs string argumet and returns string
(defn expand-path [f]
  (let [home (File. (System/getProperty "user.home"))]
    (if (.startsWith f "~")
      (str home (.substring f 1))
      f)))

(defn get-name [f]
  (if (or (instance? java.io.File f)
          (instance? java.util.zip.ZipEntry f)
          (instance? java.util.jar.JarEntry f))
    (.getName f)
    f))

(defn get-ext [f]
  (let [name (get-name f)
        dotindex (.lastIndexOf name ".")]
    (if 
      (and
        (> dotindex 0)
        (< dotindex (- (.length name) 1)))
      (.toLowerCase (last (split name #"\.")))
      "")))

(defn get-files-in-directory [d]
  (filter #(.isFile %) (.listFiles d)))

(defn has-ext? [f ext]
  (= (.toLowerCase ext) (get-ext f)))

(defn is-dot-dir? [name]
  (contains? #{"." ".."} name))

(defn split-path [f]
  (split (.getPath f) (re-pattern File/separator)))

(defn hidden? [name]
  (and
    (.startsWith name ".")
    (not (is-dot-dir? name))))

(defn hidden-dir? [d]
  (let [elems (split-path d)]
    (some #(hidden? %) elems)))

(defn hidden-file? [f]
  (hidden? (get-name f)))
