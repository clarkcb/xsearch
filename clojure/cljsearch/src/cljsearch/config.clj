(ns cljsearch.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def XSEARCHPATH
  (let [xsearchpath-env (System/getenv "XSEARCH_PATH")]
    (if (nil? xsearchpath-env)
      (clojure.string/join java.io.File/separator [(System/getenv "HOME") "src" "xsearch"])
      xsearchpath-env)))

(def SHAREDPATH
  (clojure.string/join java.io.File/separator [XSEARCHPATH "shared"]))

(def FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.json"]))

(def SEARCHOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "searchoptions.json"]))
