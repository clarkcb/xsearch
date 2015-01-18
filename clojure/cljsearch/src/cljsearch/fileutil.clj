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
  (:use [clojure.string :only (split)]))

(defn get-name [f]
  (if (or (instance? java.io.File f)
          (instance? java.util.zip.ZipEntry f)
          (instance? java.util.jar.JarEntry f))
    (.getName f)
    f))

(defn get-ext [f]
  (let [name (get-name f)]
    (if (> (.lastIndexOf name ".") 0)
      (.toLowerCase (last (split name #"\.")))
      "")))

(defn get-files-in-directory [d]
  (filter #(.isFile %) (.listFiles d)))

(defn has-ext? [f ext]
  (= (.toLowerCase ext) (get-ext f)))

(defn is-dot-dir? [name]
  (contains? #{"." ".."} name))

(defn hidden? [name]
  (and
    (.startsWith name ".")
    (not (is-dot-dir? name))))

(defn hidden-dir? [d]
  (let [elems (split (.getPath d) (re-pattern File/separator))]
    (some #(hidden? %) elems)))

(defn hidden-file? [f]
  (hidden? (get-name f)))
