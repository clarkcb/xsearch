package main

import (
	"fmt"
	"gosearch/pkg/gosearch"
	"os"
)

func errorAndExit(err error, searchOptions *gosearch.SearchOptions) {
	fmt.Printf("\nERROR: %s\n", err)
	searchOptions.PrintUsage()
}

func main() {
	searchOptions := gosearch.NewSearchOptions()
	settings, err := searchOptions.SearchSettingsFromArgs(os.Args[1:])
	if err != nil {
		errorAndExit(err, searchOptions)
	}

	if settings.PrintUsage {
		searchOptions.PrintUsage()
	}

	if settings.PrintVersion {
		searchOptions.PrintVersion()
	}

	if settings.Debug {
		fmt.Printf("settings: %s\n", settings.String())
	}

	searcher := gosearch.NewSearcher(settings)
	err = searcher.Search()
	if err != nil {
		errorAndExit(err, searchOptions)
	}

	// if there are results and PrintResults is true then print them out
	if settings.PrintResults {
		fmt.Println()
		searcher.PrintSearchResults()
	}

	// print matching dirs
	if settings.ListDirs {
		fmt.Println()
		searcher.PrintMatchingDirs()
	}

	// print matching files
	if settings.ListFiles {
		fmt.Println()
		searcher.PrintMatchingFiles()
	}

	// print matching lines (unique or not, sorted alphabetically)
	if settings.ListLines {
		fmt.Println()
		searcher.PrintMatchingLines()
	}
}
