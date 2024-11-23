package gosearch

import (
	"fmt"
	"io"
	"os"
	"strings"
	"testing"
)

func getTestFileContents() (string, error) {
	config := NewSearchConfig()
	testFile := fmt.Sprintf("%s/shared/testFiles/testFile2.txt", config.XSEARCHPATH)
	r, err1 := os.Open(testFile)
	if err1 != nil {
		return "", err1
	}
	bytes, err2 := io.ReadAll(r)
	if err2 != nil {
		return "", err2
	}
	return string(bytes), err2
}

func getSettings() *SearchSettings {
	settings := GetDefaultSearchSettings()
	settings.AddSearchPattern("Searcher")
	settings.AddPath(".")
	return settings
}

func getSearcher() *Searcher {
	settings := getSettings()
	return NewSearcher(settings)
}

/*************************************************************
 * SearchTextReaderLines test
 *************************************************************/
func TestSearchTextReaderLines(t *testing.T) {
	contents, err := getTestFileContents()
	if err != nil {
		t.Errorf("error from getTestFileContents()")
		panic(err)
	}
	searcher := getSearcher()
	results := searcher.SearchTextReaderLines(strings.NewReader(contents))

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 30
	if firstResult.LineNum != expectedFirstLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *firstResult,
			expectedFirstLineNum)
	}
	expectedFirstMatchStartIndex := 3
	if firstResult.MatchStartIndex != expectedFirstMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *firstResult,
			expectedFirstMatchStartIndex)
	}
	expectedFirstMatchEndIndex := 11
	if firstResult.MatchEndIndex != expectedFirstMatchEndIndex {
		t.Errorf("firstResult=%v, expected MatchEndIndex=%d", *firstResult,
			expectedFirstMatchEndIndex)
	}

	secondResult := results[1]
	expectedSecondLineNum := 36
	if secondResult.LineNum != expectedSecondLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *secondResult,
			expectedSecondLineNum)
	}
	expectedSecondMatchStartIndex := 24
	if secondResult.MatchStartIndex != expectedSecondMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *secondResult,
			expectedSecondMatchStartIndex)
	}
	expectedSecondMatchEndIndex := 32
	if secondResult.MatchEndIndex != expectedSecondMatchEndIndex {
		t.Errorf("secondResult=%v, expected MatchEndIndex=%d", *secondResult,
			expectedSecondMatchEndIndex)
	}
}

/*************************************************************
 * SearchMultiLineString test
 *************************************************************/
func TestSearchMultiLineString(t *testing.T) {
	contents, err := getTestFileContents()
	if err != nil {
		t.Errorf("error from getTestFileContents()")
	}
	searcher := getSearcher()
	results := searcher.SearchMultiLineString(contents)

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 30
	if firstResult.LineNum != expectedFirstLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *firstResult,
			expectedFirstLineNum)
	}
	expectedFirstMatchStartIndex := 3
	if firstResult.MatchStartIndex != expectedFirstMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *firstResult,
			expectedFirstMatchStartIndex)
	}
	expectedFirstMatchEndIndex := 11
	if firstResult.MatchEndIndex != expectedFirstMatchEndIndex {
		t.Errorf("firstResult=%v, expected MatchEndIndex=%d", *firstResult,
			expectedFirstMatchEndIndex)
	}

	secondResult := results[1]
	expectedSecondLineNum := 36
	if secondResult.LineNum != expectedSecondLineNum {
		t.Errorf("firstResult=%v, expected LineNum=%d", *secondResult,
			expectedSecondLineNum)
	}
	expectedSecondMatchStartIndex := 24
	if secondResult.MatchStartIndex != expectedSecondMatchStartIndex {
		t.Errorf("firstResult=%v, expected MatchStartIndex=%d", *secondResult,
			expectedSecondMatchStartIndex)
	}
	expectedSecondMatchEndIndex := 32
	if secondResult.MatchEndIndex != expectedSecondMatchEndIndex {
		t.Errorf("secondResult=%v, expected MatchEndIndex=%d", *secondResult,
			expectedSecondMatchEndIndex)
	}
}
