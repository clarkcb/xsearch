(ns cljsearch.cljsearch
  (:gen-class)
  (:use [cljfind.common :only (log-msg log-errors)]
        [cljsearch.searcher :only
         (search print-search-results print-search-results-matching-dirs
                 print-search-results-matching-files
                 print-search-results-matching-lines)]
        [cljsearch.searchoptions :only (settings-from-args usage)]))

(defn -main
  "This will be the main function for cljsearch"
  [& args]
  (let [[^SearchSettings settings errs] (settings-from-args args)]
    (if (:debug settings) (log-msg settings))
    (if (empty? errs)
      (do
        (if (:print-usage settings) (usage))
        (let [[results errs] (search settings)]
          (if (empty? errs)
            (do
              (if (:print-results settings) (print-search-results results settings))
              (if (:print-dirs settings) (print-search-results-matching-dirs results settings))
              (if (:print-files settings) (print-search-results-matching-files results settings))
              (if (:print-lines settings) (print-search-results-matching-lines results settings)))
            (do
              (log-errors errs (:colorize settings))
              (usage)))))
      (do
        (log-errors errs true)
        (usage)))))
