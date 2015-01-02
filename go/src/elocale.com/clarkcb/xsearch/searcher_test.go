package xsearch

import "strings"
import "testing"

func getSearchSettings() *SearchSettings {
	settings := GetDefaultSearchSettings()
	settings.AddInExtension("go")
	settings.AddSearchPattern("Searcher")
	settings.StartPath = "."
	return settings
}

func getSearcher() *Searcher {
	settings := getSearchSettings()
	return NewSearcher(settings)
}

func getMultiLineString() string {
	return "This is line 1\n" +
           "This is line 2\n" +
           "This is line 3, it includes the word Searcher\n" +
           "This is line 4\n" +
           "This is line 5\n" +
           "\n" +
           "Searcher\n" +
           "This is line 8\n" +
           "\n" +
           "The end."
}

func TestSearchTextReaderLines(t *testing.T) {
	mls := getMultiLineString()
	searcher := getSearcher()
	results := searcher.SearchTextReaderLines(strings.NewReader(mls))

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 3
	if firstResult.LineNum != expectedFirstLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *firstResult,
			expectedFirstLineNum)
	}

	secondResult := results[1]
	expectedSecondLineNum := 7
	if secondResult.LineNum != expectedSecondLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *secondResult,
			expectedSecondLineNum)
	}
}

func TestSearchMultiLineString(t *testing.T) {
	mls := getMultiLineString()
	searcher := getSearcher()
	results := searcher.SearchMultiLineString(mls)

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 3
	if firstResult.LineNum != expectedFirstLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *firstResult,
			expectedFirstLineNum)
	}

	secondResult := results[1]
	expectedSecondLineNum := 7
	if secondResult.LineNum != expectedSecondLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *secondResult,
			expectedSecondLineNum)
	}
}
