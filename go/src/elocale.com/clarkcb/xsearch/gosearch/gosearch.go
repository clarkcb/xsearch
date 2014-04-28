package main

import (
	"fmt"
	"elocale.com/clarkcb/xsearch"
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
		fmt.Printf("settings: %v\n", settings)
	}

	searcher := xsearch.NewSearcher(settings)
	err = searcher.Search()
	if err != nil {
		fmt.Println(err)
		searchOptions.PrintUsage()
	}
}
