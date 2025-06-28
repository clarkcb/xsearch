package main

import (
	"fmt"
	"gofind/pkg/gofind"
	"gosearch/pkg/gosearch"
	"os"
)

func errorAndExit(err error, searchOptions *gosearch.SearchOptions) {
	gofind.Log("")
	gofind.LogError(fmt.Sprintf("%s", err))
	searchOptions.PrintUsage()
}

func main() {
	searchOptions := gosearch.NewSearchOptions()
	settings, err := searchOptions.SearchSettingsFromArgs(os.Args[1:])
	if err != nil {
		errorAndExit(err, searchOptions)
	}

	if settings.PrintUsage() {
		searchOptions.PrintUsage()
	}

	if settings.PrintVersion() {
		searchOptions.PrintVersion()
	}

	if settings.Debug() {
		gofind.Log(fmt.Sprintf("settings: %s\n", settings.String()))
	}

	searcher := gosearch.NewSearcher(settings)
	searchResults, err := searcher.Search()
	if err != nil {
		errorAndExit(err, searchOptions)
	}
	formatter := gosearch.NewSearchResultFormatter(settings)

	// if there are results and PrintResults is true then print them out
	if settings.PrintResults() {
		gofind.Log("")
		searchResults.PrintSearchResults(formatter)
	}

	// print matching dirs
	if settings.PrintDirs() {
		searchResults.PrintMatchingDirs(formatter)
	}

	// print matching files
	if settings.PrintFiles() {
		searchResults.PrintMatchingFiles(formatter)
	}

	// print matching lines (unique or not, sorted alphabetically)
	if settings.PrintLines() {
		searchResults.PrintMatchingLines(formatter)
	}
}
