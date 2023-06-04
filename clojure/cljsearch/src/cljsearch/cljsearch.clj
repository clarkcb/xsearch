(ns cljsearch.cljsearch
  (:gen-class)
  (:use [cljfind.common :only (log-msg log-errors)]
        [cljsearch.searcher :only
          (search print-search-results print-matching-dirs print-matching-files
            print-matching-lines)]
        [cljsearch.searchoptions :only (settings-from-args usage)]))

(defn -main
  "This will be the main function for cljsearch"
  [& args]
  (let [[settings errs] (settings-from-args args)]
    (if (:debug settings) (log-msg settings))
    (if (empty? errs)
      (do
        (if (:print-usage settings) (usage))
        (let [[results errs] (search settings)]
          (if (empty? errs)
            (do
              (if (:print-results settings) (print-search-results results settings))
              (if (:list-dirs settings) (print-matching-dirs results))
              (if (:list-files settings) (print-matching-files results))
              (if (:list-lines settings) (print-matching-lines results settings)))
            (do
              (log-errors errs)
              (usage)))))
      (do
        (log-errors errs)
        (usage)))))
