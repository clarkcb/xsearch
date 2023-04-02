package gosearch

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"testing"
)

func getTestFileContents() (string, error) {
	testFile := fmt.Sprintf("%s/shared/testFiles/testFile2.txt", XSEARCHPATH)
	r, err1 := os.Open(testFile)
	if err1 != nil {
		return "", err1
	}
	bytes, err2 := ioutil.ReadAll(r)
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
 * isSearchDir tests
 *************************************************************/

func TestIsSearchDir_SingleDot_True(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	d := "."
	if !searcher.isSearchDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsSearchDir_DoubleDot_True(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	d := ".."
	if !searcher.isSearchDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsSearchDir_IsHidden_False(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	d := ".git"
	if searcher.isSearchDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsSearchDir_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.ExcludeHidden = false
	searcher := NewSearcher(settings)
	d := ".git"
	if !searcher.isSearchDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsSearchDir_NoPatterns_True(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	d := "/Users"
	if !searcher.isSearchDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsSearchDir_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("Search")
	searcher := NewSearcher(settings)
	d := "CsSearch"
	if !searcher.isSearchDir(d) {
		t.Errorf("expected true")
	}
}

func TestIsSearchDir_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("Search")
	searcher := NewSearcher(settings)
	d := "CsSearch"
	if searcher.isSearchDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsSearchDir_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInDirPattern("SearchFiles")
	searcher := NewSearcher(settings)
	d := "CsSearch"
	if searcher.isSearchDir(d) {
		t.Errorf("expected false")
	}
}

func TestIsSearchDir_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutDirPattern("SearchFiles")
	searcher := NewSearcher(settings)
	var d = "CsSearch"
	if !searcher.isSearchDir(d) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * isSearchFile tests
*************************************************************/

func TestIsSearchFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if !searcher.isSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsSearchFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if !searcher.isSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsSearchFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("java")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if searcher.isSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsSearchFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if searcher.isSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsSearchFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("java")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if !searcher.isSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsSearchFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Search")
	searcher := NewSearcher(settings)
	f := "Searcher.cs"
	if !searcher.isSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsSearchFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInFilePattern("Search")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if searcher.isSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsSearchFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Search")
	searcher := NewSearcher(settings)
	f := "Searcher.cs"
	if searcher.isSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsSearchFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutFilePattern("Search")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if !searcher.isSearchFile(f) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * isArchiveSearchFile tests
*************************************************************/

func TestIsArchiveSearchFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.isArchiveSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveSearchFile_MatchesInExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveExtension("zip")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.isArchiveSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveSearchFile_DoesNotMatchInExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveExtension("gz")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.isArchiveSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveSearchFile_MatchesOutExtension_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveExtension("zip")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.isArchiveSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveSearchFile_DoesNotMatchOutExtension_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveExtension("gz")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.isArchiveSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveSearchFile_MatchesInPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveFilePattern("arch")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.isArchiveSearchFile(f) {
		t.Errorf("expected true")
	}
}

func TestIsArchiveSearchFile_DoesNotMatchInPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddInArchiveFilePattern("archives")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.isArchiveSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveSearchFile_MatchesOutPattern_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveFilePattern("arch")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.isArchiveSearchFile(f) {
		t.Errorf("expected false")
	}
}

func TestIsArchiveSearchFile_DoesNotMatchOutPattern_True(t *testing.T) {
	settings := getSettings()
	settings.AddOutArchiveFilePattern("archives")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.isArchiveSearchFile(f) {
		t.Errorf("expected true")
	}
}

/*************************************************************
 * filterFile tests
*************************************************************/

func TestFilterFile_IsHidden_False(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	f := ".gitignore"
	if searcher.filterFile(f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_IsHiddenIncludeHidden_True(t *testing.T) {
	settings := getSettings()
	settings.ExcludeHidden = false
	searcher := NewSearcher(settings)
	f := ".gitignore"
	// fmt.Printf("isHidden(\"%s\"): %v\n", f, isHidden(f))
	// fmt.Printf("isSearchFile(\"%s\"): %v\n", f, searcher.isSearchFile(&f))
	if !searcher.filterFile(f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_ArchiveNoSearchArchives_False(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.filterFile(f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_ArchiveSearchArchives_True(t *testing.T) {
	settings := getSettings()
	settings.SearchArchives = true
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.filterFile(f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_IsArchiveSearchFile_True(t *testing.T) {
	settings := getSettings()
	settings.SearchArchives = true
	settings.AddInArchiveExtension("zip")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if !searcher.filterFile(f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_NotIsArchiveSearchFile_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("zip")
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.filterFile(f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_ArchiveFileArchivesOnly_True(t *testing.T) {
	settings := getSettings()
	settings.ArchivesOnly = true
	searcher := NewSearcher(settings)
	f := "archive.zip"
	if searcher.filterFile(f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_NoExtensionsNoPatterns_True(t *testing.T) {
	settings := getSettings()
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if !searcher.filterFile(f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_isSearchFile_True(t *testing.T) {
	settings := getSettings()
	settings.AddInExtension("cs")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if !searcher.filterFile(f) {
		t.Errorf("expected true")
	}
}

func TestFilterFile_NotisSearchFile_False(t *testing.T) {
	settings := getSettings()
	settings.AddOutExtension("cs")
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if searcher.filterFile(f) {
		t.Errorf("expected false")
	}
}

func TestFilterFile_NonArchiveFileArchivesOnly_False(t *testing.T) {
	settings := getSettings()
	settings.ArchivesOnly = true
	searcher := NewSearcher(settings)
	f := "FileUtil.cs"
	if searcher.filterFile(f) {
		t.Errorf("expected false")
	}
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
	settings := getSettings()
	searcher := NewSearcher(settings)
	results := searcher.SearchTextReaderLines(strings.NewReader(contents))

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 29
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
	expectedSecondLineNum := 35
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
	settings := getSettings()
	searcher := NewSearcher(settings)
	results := searcher.SearchMultiLineString(contents)

	if len(results) != 2 {
		t.Errorf("len(results)=%d, expected=2", len(results))
	}

	firstResult := results[0]
	expectedFirstLineNum := 29
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
	expectedSecondLineNum := 35
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
