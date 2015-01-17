(ns cljsearch.cljsearch
  (:gen-class)
  (:use cljsearch.common)
  (:use cljsearch.searchoptions)
  (:use cljsearch.searchsettings))

(defn -main
  "This will be the main function for cljsearch"
  [& args]
  (let [[settings errs] (settings-from-args DEFAULT-SETTINGS args [])]
    (if (empty? errs)
      (do
        (if (:debug settings) (log-msg settings))
        (if (:printusage settings) (usage))
      )
      (do
        (log-errors errs)
        (usage))
    )))
