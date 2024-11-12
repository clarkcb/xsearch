package gosearch

import (
	"bytes"
	"fmt"
	"gofind/pkg/gofind"
	"regexp"
	"sort"
	"strings"
	"unicode"
)

type SearchResultsStats struct {
	DirCounts     map[string]int
	FileCounts    map[string]int
	LineCounts    map[string]int
	PatternCounts map[string]int
}

func NewSearchResultsStats() *SearchResultsStats {
	return &SearchResultsStats{
		make(map[string]int),
		make(map[string]int),
		make(map[string]int),
		make(map[string]int),
	}
}

func (s *SearchResultsStats) Clear() {
	s.DirCounts = make(map[string]int)
	s.FileCounts = make(map[string]int)
	s.LineCounts = make(map[string]int)
	s.PatternCounts = make(map[string]int)
}

type SearchResults struct {
	Settings      *SearchSettings
	FileResults   *gofind.FileResults
	SearchResults []*SearchResult
	Stats         *SearchResultsStats
}

func NewSearchResults(settings *SearchSettings) *SearchResults {
	return &SearchResults{
		settings,
		gofind.NewFileResults(),
		[]*SearchResult{},
		NewSearchResultsStats(),
	}
}

//func NewSearchResults(settings *SearchSettings, fileResults *gofind.FileResults) *SearchResults {
//	return &SearchResults{
//		settings,
//		fileResults,
//		[]*SearchResult{},
//		NewSearchResultsStats(),
//	}
//}

func (srs *SearchResults) AddSearchResultStats(r *SearchResult) {
	srs.Stats.DirCounts[r.File.Path]++
	srs.Stats.FileCounts[r.File.String()]++
	if strings.TrimSpace(r.Line) != "" {
		srs.Stats.LineCounts[strings.TrimSpace(r.Line)]++
	}
	srs.Stats.PatternCounts[r.Pattern.String()]++
}

func (srs *SearchResults) AddSearchResult(r *SearchResult) {
	srs.SearchResults = append(srs.SearchResults, r)
	srs.AddSearchResultStats(r)
}

func (srs *SearchResults) Clear() {
	srs.FileResults = nil
	srs.SearchResults = nil
	srs.Stats.Clear()
}

func (srs *SearchResults) IsEmpty() bool {
	return len(srs.SearchResults) == 0
}

func (srs *SearchResults) HasResultForFileAndPattern(fr *gofind.FileResult,
	pattern *regexp.Regexp) bool {
	for _, r := range srs.SearchResults {
		if pattern.String() == r.Pattern.String() && fr.String() == r.File.String() {
			return true
		}
	}
	return false
}

// methods for the sort.Interface interface (so you can call sort.Sort on
// SearchResults instances)
func (srs *SearchResults) Len() int {
	return len(srs.SearchResults)
}

func (srs *SearchResults) Less(i, j int) bool {
	sri, srj := srs.SearchResults[i], srs.SearchResults[j]
	// file results should already be in the right order, so we check their indices
	fri := srs.FileResults.Index(sri.File)
	frj := srs.FileResults.Index(srj.File)
	if fri != frj {
		return fri < frj
	}
	sres := compareSearchFields(sri, srj)
	if srs.Settings.FindSettings.SortDescending() {
		return sres >= 0
	}
	return sres < 0
}

func (srs *SearchResults) Swap(i, j int) {
	srs.SearchResults[j], srs.SearchResults[i] = srs.SearchResults[i], srs.SearchResults[j]
}

func compareSearchFields(sr1, sr2 *SearchResult) int {
	if sr1.LineNum == sr2.LineNum {
		if sr1.MatchStartIndex == sr2.MatchStartIndex {
			if sr1.MatchEndIndex == sr2.MatchEndIndex {
				return 0
			}
			if sr1.MatchEndIndex < sr2.MatchEndIndex {
				return -1
			}
			return 1
		}
		if sr1.MatchStartIndex < sr2.MatchStartIndex {
			return -1
		}
		return 1
	}
	if sr1.LineNum < sr2.LineNum {
		return -1
	}
	return 1
}

func (srs *SearchResults) CompareByPath(sr1, sr2 *SearchResult, sortCaseInsensitive bool) int {
	pres := srs.FileResults.CompareByPath(sr1.File, sr2.File, sortCaseInsensitive)
	if pres == 0 {
		return compareSearchFields(sr1, sr2)
	}
	if pres < 0 {
		return -1
	}
	return 1
}

func (srs *SearchResults) getSortByPath(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		return srs.CompareByPath(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) CompareByName(sr1, sr2 *SearchResult, sortCaseInsensitive bool) int {
	nres := srs.FileResults.CompareByName(sr1.File, sr2.File, sortCaseInsensitive)
	if nres == 0 {
		return compareSearchFields(sr1, sr2)
	}
	if nres < 0 {
		return -1
	}
	return 1
}

func (srs *SearchResults) getSortByName(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		return srs.CompareByName(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) CompareBySize(sr1, sr2 *SearchResult, sortCaseInsensitive bool) int {
	nres := srs.FileResults.CompareBySize(sr1.File, sr2.File, sortCaseInsensitive)
	if nres == 0 {
		return compareSearchFields(sr1, sr2)
	}
	if nres < 0 {
		return -1
	}
	return 1
}

func (srs *SearchResults) getSortBySize(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		return srs.CompareBySize(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) CompareByType(sr1, sr2 *SearchResult, sortCaseInsensitive bool) int {
	tres := srs.FileResults.CompareByType(sr1.File, sr2.File, sortCaseInsensitive)
	if tres == 0 {
		return compareSearchFields(sr1, sr2)
	}
	if tres < 0 {
		return -1
	}
	return 1
}

func (srs *SearchResults) getSortByType(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		return srs.CompareByType(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) CompareByLastMod(sr1, sr2 *SearchResult, sortCaseInsensitive bool) int {
	lres := srs.FileResults.CompareByLastMod(sr1.File, sr2.File, sortCaseInsensitive)
	if lres == 0 {
		return compareSearchFields(sr1, sr2)
	}
	if lres < 0 {
		return -1
	}
	return 1
}

func (srs *SearchResults) getSortByLastMod(sortCaseInsensitive bool) func(i, j int) bool {
	return func(i, j int) bool {
		return srs.CompareByLastMod(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) Sort(settings *SearchSettings) {
	switch settings.SortBy() {
	case gofind.SortByFileName:
		sort.Slice(srs.FileResults, srs.getSortByName(settings.SortCaseInsensitive()))
	case gofind.SortByFileSize:
		sort.Slice(srs.FileResults, srs.getSortBySize(settings.SortCaseInsensitive()))
	case gofind.SortByFileType:
		sort.Slice(srs.FileResults, srs.getSortByType(settings.SortCaseInsensitive()))
	case gofind.SortByLastMod:
		sort.Slice(srs.FileResults, srs.getSortByLastMod(settings.SortCaseInsensitive()))
	default:
		sort.Slice(srs.FileResults, srs.getSortByPath(settings.SortCaseInsensitive()))
	}
	if settings.SortDescending() {
		srs.reverse()
	}
}

func (srs *SearchResults) reverse() {
	for i, j := 0, len(srs.SearchResults)-1; i < j; i, j = i+1, j-1 {
		srs.SearchResults[i], srs.SearchResults[j] = srs.SearchResults[j], srs.SearchResults[i]
	}
}

func (srs *SearchResults) GetPathCountMap() map[string]int {
	pathCountMap := make(map[string]int)
	for _, r := range srs.SearchResults {
		pathCountMap[r.File.Path]++
	}
	return pathCountMap
}

func (srs *SearchResults) GetMatchingDirs() []string {
	pathCountMap := srs.GetPathCountMap()
	paths := gofind.GetSortedCountKeys(pathCountMap)
	return paths
}

func (srs *SearchResults) PrintMatchingDirs() {
	paths := srs.GetMatchingDirs()
	if len(paths) > 0 {
		gofind.Log(fmt.Sprintf("\nMatching directories (%d):", len(paths)))
		for _, p := range paths {
			gofind.Log(p)
		}
	} else {
		gofind.Log("\nMatching directories: 0")
	}
}

func (srs *SearchResults) GetFileCountMap() map[string]int {
	fileCountMap := make(map[string]int)
	for _, r := range srs.SearchResults {
		fileCountMap[r.File.String()]++
	}
	return fileCountMap
}

func (srs *SearchResults) GetMatchingFiles() []string {
	fileCountMap := srs.GetFileCountMap()
	files := gofind.GetSortedCountKeys(fileCountMap)
	return files
}

func (srs *SearchResults) PrintMatchingFiles() {
	files := srs.GetMatchingFiles()
	if len(files) > 0 {
		gofind.Log(fmt.Sprintf("\nMatching files (%d):", len(files)))
		for _, p := range files {
			gofind.Log(p)
		}
	} else {
		gofind.Log("\nMatching files: 0")
	}
}

func (srs *SearchResults) PrintMatchingLines() {
	countMap := srs.Stats.LineCounts
	totalCount := 0
	for _, v := range countMap {
		totalCount += v
	}
	countKeys := gofind.GetCaseInsensitiveSortedCountKeys(countMap)

	if srs.Settings.UniqueLines() {
		gofind.Log(fmt.Sprintf("\nUnique lines with matches (%d):", len(countKeys)))
		for _, k := range countKeys {
			gofind.Log(fmt.Sprintf("%s", k))
		}
	} else {
		gofind.Log(fmt.Sprintf("\nLines with matches (%d):", totalCount))
		for _, k := range countKeys {
			for i := 0; i < countMap[k]; i++ {
				gofind.Log(fmt.Sprintf("%s", k))
			}
		}
	}
}

func printCounts(pluralName string, countKeys []string) {
	gofind.Log(fmt.Sprintf("%s with matches (%d):", strings.Title(pluralName),
		len(countKeys)))
	for _, k := range countKeys {
		gofind.Log(fmt.Sprintf("%s", k))
	}
}

func (srs *SearchResults) PrintDirCounts() {
	countMap := srs.Stats.DirCounts
	countKeys := gofind.GetSortedCountKeys(countMap)
	printCounts("directories", countKeys)
}

func (srs *SearchResults) PrintFileCounts() {
	countMap := srs.Stats.FileCounts
	countKeys := gofind.GetSortedCountKeys(countMap)
	printCounts("files", countKeys)
}

func (srs *SearchResults) PrintLineCounts() {
	countMap := srs.Stats.LineCounts
	totalCount := 0
	for _, v := range countMap {
		totalCount += v
	}
	countKeys := gofind.GetCaseInsensitiveSortedCountKeys(countMap)
	gofind.Log(fmt.Sprintf("Lines with matches (%d):", totalCount))
	for _, k := range countKeys {
		for i := 0; i < countMap[k]; i++ {
			gofind.Log(fmt.Sprintf("%s", k))
		}
	}
}

func (srs *SearchResults) PrintUniqueLineCounts() {
	countMap := srs.Stats.LineCounts
	countKeys := gofind.GetCaseInsensitiveSortedCountKeys(countMap)
	gofind.Log(fmt.Sprintf("Unique lines with matches (%d):", len(countKeys)))
	for _, k := range countKeys {
		gofind.Log(fmt.Sprintf("%s", k))
	}
}

func (srs *SearchResults) PrintPatternCounts(patterns []string) {
	printCounts("patterns", patterns)
}

func (srs *SearchResults) PrintSearchResults() {
	formatter := NewSearchResultFormatter(srs.Settings)
	gofind.Log(fmt.Sprintf("Search results (%d):", srs.Len()))
	for _, r := range srs.SearchResults {
		if len(srs.Stats.PatternCounts) > 1 {
			gofind.Log(fmt.Sprintf("\"%s\": ", r.Pattern.String()))
		}
		gofind.Log(formatter.Format(r))
	}
}

type SearchResultFormatter struct {
	Settings *SearchSettings
}

func NewSearchResultFormatter(settings *SearchSettings) *SearchResultFormatter {
	return &SearchResultFormatter{
		settings,
	}
}

func (f *SearchResultFormatter) Format(r *SearchResult) string {
	if len(r.LinesBefore) > 0 || len(r.LinesAfter) > 0 {
		return f.multiLineFormat(r)
	} else {
		return f.singleLineFormat(r)
	}
}

const SEPARATOR_LEN = 80

func lineNumPadding(r *SearchResult) int {
	return len(fmt.Sprintf("%d", r.LineNum+len(r.LinesAfter)))
}

func colorize(s string, matchStartIndex int, matchEndIndex int) string {
	return s[0:matchStartIndex] +
		COLOR_GREEN +
		s[matchStartIndex:matchEndIndex] +
		COLOR_RESET +
		s[matchEndIndex:]
}

func (f *SearchResultFormatter) multiLineFormat(r *SearchResult) string {
	var buffer bytes.Buffer
	buffer.WriteString(strings.Repeat("=", SEPARATOR_LEN))
	buffer.WriteString(fmt.Sprintf("\n%s: %d: [%d:%d]\n", r.File.String(),
		r.LineNum, r.MatchStartIndex, r.MatchEndIndex))
	buffer.WriteString(strings.Repeat("-", SEPARATOR_LEN))
	buffer.WriteString("\n")
	lineFormat := fmt.Sprintf(" %%%dd | %%s\n", lineNumPadding(r))
	currentLineNum := r.LineNum
	if len(r.LinesBefore) > 0 {
		currentLineNum -= len(r.LinesBefore)
		for _, l := range r.LinesBefore {
			buffer.WriteString(" " + fmt.Sprintf(lineFormat, currentLineNum, l))
			currentLineNum++
		}
	}
	line := r.Line
	if f.Settings.Colorize() {
		line = colorize(line, r.MatchStartIndex-1, r.MatchEndIndex-1)
	}
	buffer.WriteString(">" + fmt.Sprintf(lineFormat, currentLineNum, line))
	if len(r.LinesAfter) > 0 {
		currentLineNum++
		for _, l := range r.LinesAfter {
			buffer.WriteString(" " + fmt.Sprintf(lineFormat, currentLineNum, l))
			currentLineNum++
		}
	}
	return buffer.String()
}

func (f *SearchResultFormatter) singleLineFormat(r *SearchResult) string {
	if r.LineNum > 0 {
		return fmt.Sprintf("%s: %d: [%d:%d]: %s", r.File.String(), r.LineNum,
			r.MatchStartIndex, r.MatchEndIndex, f.formatMatchingLine(r))
	} else {
		return fmt.Sprintf("%s matches at [%d:%d]", r.File.String(),
			r.MatchStartIndex, r.MatchEndIndex)
	}
}

func (f *SearchResultFormatter) formatMatchingLine(r *SearchResult) string {
	formatted := r.Line
	leadingWhitespaceCount := 0

	for _, c := range formatted {
		if unicode.IsSpace(c) {
			leadingWhitespaceCount += 1
		} else {
			break
		}
	}
	formatted = strings.TrimSpace(formatted)
	formattedLength := len(formatted)
	maxLineEndIndex := formattedLength - 1
	matchLength := r.MatchEndIndex - r.MatchStartIndex
	matchStartIndex := r.MatchStartIndex - 1 - leadingWhitespaceCount
	matchEndIndex := matchStartIndex + matchLength

	if formattedLength > f.Settings.MaxLineLength() {
		lineStartIndex := matchStartIndex
		lineEndIndex := lineStartIndex + matchLength
		matchStartIndex = 0
		matchEndIndex = matchLength

		for lineEndIndex > formattedLength-1 {
			lineStartIndex -= 1
			lineEndIndex -= 1
			matchStartIndex += 1
			matchEndIndex += 1
		}

		formattedLength = lineEndIndex - lineStartIndex
		for formattedLength < f.Settings.MaxLineLength() {
			if lineStartIndex > 0 {
				lineStartIndex -= 1
				matchStartIndex += 1
				matchEndIndex += 1
				formattedLength = lineEndIndex - lineStartIndex
			}
			if formattedLength < f.Settings.MaxLineLength() && lineEndIndex < maxLineEndIndex {
				lineEndIndex += 1
			}
			formattedLength = lineEndIndex - lineStartIndex
		}

		formatted = formatted[lineStartIndex:lineEndIndex]
		if lineStartIndex > 2 {
			formatted = "..." + formatted[3:]
		}
		if lineEndIndex < maxLineEndIndex-3 {
			formatted = formatted[0:len(formatted)-3] + "..."
		}
	}

	if f.Settings.Colorize() {
		formatted = colorize(formatted, matchStartIndex, matchEndIndex)
	}
	return formatted
}

type SearchResult struct {
	Pattern         *regexp.Regexp
	File            *gofind.FileResult
	LineNum         int
	MatchStartIndex int
	MatchEndIndex   int
	Line            string
	LinesBefore     []string
	LinesAfter      []string
}

func (r *SearchResult) Text() string {
	var buffer bytes.Buffer
	for _, l := range r.LinesBefore {
		buffer.WriteString(fmt.Sprintf("%s\n", l))
	}
	buffer.WriteString(fmt.Sprintf("%s\n", r.Line))
	for _, l := range r.LinesAfter {
		buffer.WriteString(fmt.Sprintf("%s\n", l))
	}
	return buffer.String()
}
