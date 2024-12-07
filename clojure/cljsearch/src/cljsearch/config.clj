(ns cljsearch.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def ^:const ^String XSEARCHPATH
  (or
   (System/getenv "XSEARCH_PATH")
   (clojure.string/join java.io.File/separator [(System/getenv "HOME") "src" "xsearch"])))

(def ^:const ^String SHAREDPATH
  (clojure.string/join java.io.File/separator [XSEARCHPATH "shared"]))

(def ^:const ^String FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.json"]))

(def ^:const ^String SEARCHOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "searchoptions.json"]))
