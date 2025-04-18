package gosearch

import (
	"fmt"
	"gofind/pkg/gofind"
	"regexp"
	"testing"
	"time"
)

func TestAddSearchResult(t *testing.T) {
	config := NewSearchConfig()
	settings := GetDefaultSearchSettings()
	settings.SetColorize(false)
	searchResults := NewSearchResults(settings)

	path := fmt.Sprintf("%s/go/gosearch/pkg/gosearch", config.XSEARCHPATH)
	name := "searchresults_test.go"
	//searchItem := NewSearchItem(path, name, FiletypeCode)
	fileResult := gofind.NewFileResult(path, name, gofind.FileTypeCode, 0, time.Time{})

	line := "func TestAddSearchResult(t *testing.T) {"

	searchResult := &SearchResult{
		regexp.MustCompile("^func\\s+TestAddSearchResult"),
		fileResult,
		6,
		1,
		25,
		line,
		[]string{},
		[]string{},
	}
	searchResults.AddSearchResult(searchResult)

	if searchResults.IsEmpty() {
		t.Errorf("searchResults.IsEmpty()=%t, expected=false",
			searchResults.IsEmpty())
	}

	if len(searchResults.SearchResults) != 1 {
		t.Errorf("len(searchResults.SearchResults)=%d, expected=1",
			len(searchResults.SearchResults))
	}

	//expectedDir := fmt.Sprintf("%s/go/gosearch/pkg/gosearch", XSEARCHPATH)
	//c, b := searchResults.DirCounts[expectedDir]
	//if !b {
	//	t.Errorf("searchResults.DirCounts does not contain expected dir key: %s",
	//		expectedDir)
	//}
	//if c != 1 {
	//	t.Errorf("searchResults.DirCounts[\"%s\"]=%d, expected=1",
	//		expectedDir, searchResults.DirCounts[expectedDir])
	//}
	//
	//expectedFile := fmt.Sprintf("%s/go/gosearch/pkg/gosearch/searchresults_test.go", XSEARCHPATH)
	//c, b = searchResults.FileCounts[expectedFile]
	//if !b {
	//	t.Errorf("searchResults.FileCounts does not contain expected file key: %s",
	//		expectedFile)
	//}
	//if c != 1 {
	//	t.Errorf("searchResults.FileCounts[\"%s\"]=%d, expected=1",
	//		expectedFile, searchResults.FileCounts[expectedFile])
	//}
}
