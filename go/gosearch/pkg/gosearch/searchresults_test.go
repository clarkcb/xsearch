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
}

func TestSingleLineSearchResult(t *testing.T) {
	pattern := "Search"
	path := "~/src/xsearch/csharp/CsSearch/CsSearchLib"
	name := "Searcher.cs"
	fileResult := gofind.NewFileResult(path, name, gofind.FileTypeCode, 0, time.Time{})
	lineNum := 14
	matchStartIndex := 14
	matchEndIndex := 20
	line := "public class Searcher\n"

	searchResult := &SearchResult{
		regexp.MustCompile(pattern),
		fileResult,
		lineNum,
		matchStartIndex,
		matchEndIndex,
		line,
		[]string{},
		[]string{},
	}

	if searchResult.LineNum != lineNum {
		t.Errorf("TestSingleLineSearchResult: searchResult.LineNum != %v", lineNum)
	}

	if searchResult.MatchStartIndex != matchStartIndex {
		t.Errorf("TestSingleLineSearchResult: searchResult.MatchStartIndex != %v", matchStartIndex)
	}

	if searchResult.MatchEndIndex != matchEndIndex {
		t.Errorf("TestSingleLineSearchResult: searchResult.MatchEndIndex != %v", matchEndIndex)
	}

	if searchResult.Line != line {
		t.Errorf("TestSingleLineSearchResult: searchResult.Line != %v", line)
	}

	settings := GetDefaultSearchSettings()
	formatter := NewSearchResultFormatter(settings)

	expectedOutput := fmt.Sprintf("%s: %d: [%d:%d]: %s",
		formatter.FileFormatter.FormatFileResult(fileResult),
		lineNum,
		matchStartIndex,
		matchEndIndex,
		formatter.formatMatchingLine(searchResult))

	output := formatter.Format(searchResult)

	if output != expectedOutput {
		t.Errorf("TestSingleLineSearchResult: output != %v", expectedOutput)
	}
}

func TestSingleLineSearchResultLongerThanMaxLineLength(t *testing.T) {
	pattern := "maxlen"
	path := "."
	name := "maxlen.txt"
	fileResult := gofind.NewFileResult(path, name, gofind.FileTypeText, 0, time.Time{})
	lineNum := 1
	matchStartIndex := 53
	matchEndIndex := 59
	line := "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"

	searchResult := &SearchResult{
		regexp.MustCompile(pattern),
		fileResult,
		lineNum,
		matchStartIndex,
		matchEndIndex,
		line,
		[]string{},
		[]string{},
	}

	settings := GetDefaultSearchSettings()
	settings.SetMaxLineLength(100)
	settings.SetColorize(false)
	formatter := NewSearchResultFormatter(settings)

	expectedLine := "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."

	expectedOutput := fmt.Sprintf("%s: %d: [%d:%d]: %s",
		formatter.FileFormatter.FormatFileResult(fileResult),
		lineNum,
		matchStartIndex,
		matchEndIndex,
		expectedLine)

	output := formatter.Format(searchResult)

	if output != expectedOutput {
		t.Errorf("TestSingleLineSearchResultLongerThanMaxLineLength: output != %v", expectedOutput)
	}
}

func TestSingleLineSearchResultLongerThanMaxLineLengthColorize(t *testing.T) {
	pattern := "maxlen"
	path := "."
	name := "maxlen.txt"
	fileResult := gofind.NewFileResult(path, name, gofind.FileTypeText, 0, time.Time{})
	lineNum := 1
	matchStartIndex := 53
	matchEndIndex := 59
	line := "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"

	searchResult := &SearchResult{
		regexp.MustCompile(pattern),
		fileResult,
		lineNum,
		matchStartIndex,
		matchEndIndex,
		line,
		[]string{},
		[]string{},
	}

	settings := GetDefaultSearchSettings()
	settings.SetMaxLineLength(100)
	settings.SetColorize(true)
	formatter := NewSearchResultFormatter(settings)

	expectedLine := "...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901..."
	colorizedLine := colorize(expectedLine, 47, 53, settings.LineColor())

	expectedOutput := fmt.Sprintf("%s: %d: [%d:%d]: %s",
		formatter.FileFormatter.FormatFileResult(fileResult),
		lineNum,
		matchStartIndex,
		matchEndIndex,
		colorizedLine)

	output := formatter.Format(searchResult)

	if output != expectedOutput {
		t.Errorf("TestSingleLineSearchResultLongerThanMaxLineLengthColorize: output != %v", expectedOutput)
	}
}

func TestSearchResultMatchLongerThanMaxLineLengthColorize(t *testing.T) {
	pattern := "\\d+maxlen\\d+"
	path := "."
	name := "maxlen.txt"
	fileResult := gofind.NewFileResult(path, name, gofind.FileTypeText, 0, time.Time{})
	lineNum := 1
	matchStartIndex := 1
	matchEndIndex := 110
	line := "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789"

	searchResult := &SearchResult{
		regexp.MustCompile(pattern),
		fileResult,
		lineNum,
		matchStartIndex,
		matchEndIndex,
		line,
		[]string{},
		[]string{},
	}

	settings := GetDefaultSearchSettings()
	settings.SetMaxLineLength(100)
	settings.SetColorize(true)
	formatter := NewSearchResultFormatter(settings)

	expectedLine := "0123456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456..."
	colorizedLine := colorize(expectedLine, 0, 97, settings.LineColor())

	expectedOutput := fmt.Sprintf("%s: %d: [%d:%d]: %s",
		formatter.FileFormatter.FormatFileResult(fileResult),
		lineNum,
		matchStartIndex,
		matchEndIndex,
		colorizedLine)

	output := formatter.Format(searchResult)

	if output != expectedOutput {
		t.Errorf("TestSearchResultMatchLongerThanMaxLineLengthColorize: output != %v", expectedOutput)
	}
}

func TestSearchResult2MatchLongerThanMaxLineLengthColorize(t *testing.T) {
	pattern := "\\d+maxlen\\d+"
	path := "."
	name := "maxlen.txt"
	fileResult := gofind.NewFileResult(path, name, gofind.FileTypeText, 0, time.Time{})
	lineNum := 1
	matchStartIndex := 11
	matchEndIndex := 120
	line := "ABCDEFGHIJ0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789ABCDEFGHIJ"

	searchResult := &SearchResult{
		regexp.MustCompile(pattern),
		fileResult,
		lineNum,
		matchStartIndex,
		matchEndIndex,
		line,
		[]string{},
		[]string{},
	}

	settings := GetDefaultSearchSettings()
	settings.SetMaxLineLength(100)
	settings.SetColorize(true)
	formatter := NewSearchResultFormatter(settings)

	expectedLine := "...3456789012345678901234567890123456789012345678901maxlen890123456789012345678901234567890123456..."
	colorizedLine := colorize(expectedLine, 3, 97, settings.LineColor())

	expectedOutput := fmt.Sprintf("%s: %d: [%d:%d]: %s",
		formatter.FileFormatter.FormatFileResult(fileResult),
		lineNum,
		matchStartIndex,
		matchEndIndex,
		colorizedLine)

	output := formatter.Format(searchResult)

	if output != expectedOutput {
		t.Errorf("TestSearchResult2MatchLongerThanMaxLineLengthColorize: output != %v", expectedOutput)
	}
}
