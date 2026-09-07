(ns cljsearch.searchconfig
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]
        [cljfind.findconfig :only (default-find-config)]))

(def ^:const ^String DEFAULT-XSEARCH-CONFIG-DIR
  (clojure.string/join java.io.File/separator [(System/getenv "HOME") ".config" "xsearch"]))

(defn get-xsearch-config-dir ^String []
  (or
   (System/getenv "XSEARCH_CONFIG_DIR")
   DEFAULT-XSEARCH-CONFIG-DIR))

(def ^:const ^String DEFAULT-XSEARCH-PATH
  (clojure.string/join java.io.File/separator [(System/getenv "HOME") "src" "xsearch"]))

(defn get-xsearch-path ^String []
  (or
   (System/getenv "XSEARCH_PATH")
   DEFAULT-XSEARCH-PATH))

(defn get-shared-path ^String []
  (clojure.string/join java.io.File/separator [(get-xsearch-path) "shared"]))

(def ^:const ^String SEARCH-OPTIONS-NAME
  "searchoptions.json")

(defn get-search-options-path ^String []
  (clojure.string/join java.io.File/separator [(get-shared-path) SEARCH-OPTIONS-NAME]))

(defn get-default-search-settings-path []
  (let [xsearch-config-dir (get-xsearch-config-dir)]
    (clojure.string/join java.io.File/separator [xsearch-config-dir "settings.json"])))

(defrecord SearchConfig [find-config search-options-path default-search-settings-path])

(defn default-search-config []
  (->SearchConfig (default-find-config) (get-search-options-path) (get-default-search-settings-path)))
