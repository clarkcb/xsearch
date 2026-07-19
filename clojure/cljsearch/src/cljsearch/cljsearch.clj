(ns cljsearch.cljsearch
  (:gen-class)
  (:use [cljfind.common :only (log-msg log-errors)]
        [cljsearch.searcher :only
         (create-searcher search
          print-search-results print-search-results-matching-dirs
          print-search-results-matching-files
          print-search-results-matching-lines
          print-search-results-matches)]
        [cljsearch.searchoptions :only (settings-from-args usage)]))

(defn -main
  "This will be the main function for cljsearch"
  [& args]
  (let [[^SearchSettings settings errs] (settings-from-args args)]
    (if (:debug settings) (log-msg settings))
    (if (empty? errs)
      (do
        (if (:print-usage settings) (usage))
        (let [searcher (create-searcher settings)
              [results errs] (search searcher)]
          (if (empty? errs)
            (do
              (if (:print-results settings) (print-search-results searcher results))
              (if (:print-dirs settings) (print-search-results-matching-dirs searcher results))
              (if (:print-files settings) (print-search-results-matching-files searcher results))
              (if (:print-lines settings) (print-search-results-matching-lines searcher results))
              (if (:print-matches settings) (print-search-results-matches searcher results)))
            (do
              (log-errors errs (:colorize settings))
              (usage)))))
      (do
        (log-errors errs true)
        (usage)))))
