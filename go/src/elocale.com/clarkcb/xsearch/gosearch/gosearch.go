package main

import (
	"elocale.com/clarkcb/xsearch"
	"fmt"
	"os"
)

func main() {
	searchOptions := xsearch.NewSearchOptions()
	settings, err := searchOptions.SearchSettingsFromArgs(os.Args[1:])
	if err != nil {
		fmt.Println(err)
		searchOptions.PrintUsage()
	}

	if settings.PrintUsage {
		searchOptions.PrintUsage()
	}

	if settings.Debug {
		fmt.Printf("settings: %s\n", settings.String())
	}

	searcher := xsearch.NewSearcher(settings)
	err = searcher.Search()
	if err != nil {
		fmt.Printf("\nERROR: %s\n", err)
		searchOptions.PrintUsage()
	}

	// if there are results and PrintResults is true then print them out
	if settings.PrintResults {
		fmt.Println()
		searcher.PrintSearchResults()
	}

	if settings.ListDirs {
		fmt.Println()
		searcher.PrintDirCounts()
	}

	if settings.ListFiles {
		fmt.Println()
		searcher.PrintFileCounts()
	}

	if settings.ListLines {
		fmt.Println()
		if settings.UniqueLines {
			searcher.PrintUniqueLineCounts()
		} else {
			searcher.PrintLineCounts()
		}
	}
}
