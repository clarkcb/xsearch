package gosearch

import (
	"bytes"
	"fmt"
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
	SearchResults []*SearchResult
	Stats         *SearchResultsStats
}

func NewSearchResults(settings *SearchSettings) *SearchResults {
	return &SearchResults{
		settings,
		[]*SearchResult{},
		NewSearchResultsStats(),
	}
}

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
	srs.SearchResults = nil
	srs.Stats.Clear()
}

func (srs *SearchResults) IsEmpty() bool {
	return len(srs.SearchResults) == 0
}

func (srs *SearchResults) HasResultForFileAndPattern(si *SearchItem,
	pattern *regexp.Regexp) bool {
	for _, r := range srs.SearchResults {
		if pattern.String() == r.Pattern.String() && si.String() == r.File.String() {
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
	sr1, sr2 := srs.SearchResults[i], srs.SearchResults[j]
	path1, path2 := strings.ToLower(sr1.File.Path), strings.ToLower(sr2.File.Path)
	pathCmp := bytes.Compare([]byte(path1), []byte(path2))
	if pathCmp > 0 {
		return false
	}
	if pathCmp == 0 {
		file1, file2 := strings.ToLower(sr1.File.Name), strings.ToLower(sr2.File.Name)
		fileCmp := bytes.Compare([]byte(file1), []byte(file2))
		if fileCmp > 0 {
			return false
		}
		if fileCmp == 0 {
			if sr1.LineNum == sr2.LineNum {
				return sr1.MatchStartIndex < sr2.MatchStartIndex
			}
			return sr1.LineNum < sr2.LineNum
		}
		return true
	}
	return true
}

func (srs *SearchResults) Swap(i, j int) {
	srs.SearchResults[j], srs.SearchResults[i] = srs.SearchResults[i], srs.SearchResults[j]
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
	paths := getSortedCountKeys(pathCountMap)
	return paths
}

func (srs *SearchResults) PrintMatchingDirs() {
	paths := srs.GetMatchingDirs()
	if len(paths) > 0 {
		log(fmt.Sprintf("\nMatching directories (%d):", len(paths)))
		for _, p := range paths {
			log(p)
		}
	} else {
		log("\nMatching directories: 0")
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
	files := getSortedCountKeys(fileCountMap)
	return files
}

func (srs *SearchResults) PrintMatchingFiles() {
	files := srs.GetMatchingFiles()
	if len(files) > 0 {
		log(fmt.Sprintf("\nMatching files (%d):", len(files)))
		for _, p := range files {
			log(p)
		}
	} else {
		log("\nMatching files: 0")
	}
}

func (srs *SearchResults) PrintMatchingLines() {
	countMap := srs.Stats.LineCounts
	totalCount := 0
	for _, v := range countMap {
		totalCount += v
	}
	countKeys := getCaseInsensitiveSortedCountKeys(countMap)

	if srs.Settings.UniqueLines {
		log(fmt.Sprintf("Unique lines with matches (%d):", len(countKeys)))
		for _, k := range countKeys {
			log(fmt.Sprintf("%s", k))
		}
	} else {
		log(fmt.Sprintf("Lines with matches (%d):", totalCount))
		for _, k := range countKeys {
			for i := 0; i < countMap[k]; i++ {
				log(fmt.Sprintf("%s", k))
			}
		}
	}
}

//func getSortedCountKeys(m map[string]int) []string {
//	mk := make([]string, len(m))
//	i := 0
//	for k, _ := range m {
//		mk[i] = k
//		i++
//	}
//	sort.Strings(mk)
//	return mk
//}

// func printCounts(singName string, pluralName string, countKeys []string,
// 	countMap map[string]int) {
// 	countName := pluralName
// 	if len(countKeys) == 1 {
// 		countName = singName
// 	}
// 	log(fmt.Sprintf("%s match counts (%d %s):\n", strings.Title(singName),
// 		len(countKeys), countName))
// 	longestKeyLen := getLongestLen(countKeys) + 1
// 	longestNumLen := getNumLen2(getHighestMapVal(countMap))
// 	lineFormat := fmt.Sprintf("%%-%ds %%%dd", longestKeyLen, longestNumLen)
// 	for _, k := range countKeys {
// 		log(fmt.Sprintf(lineFormat, k+":", countMap[k]))
// 	}
// }

func printCounts(pluralName string, countKeys []string) {
	log(fmt.Sprintf("%s with matches (%d):", strings.Title(pluralName),
		len(countKeys)))
	for _, k := range countKeys {
		log(fmt.Sprintf("%s", k))
	}
}

func (srs *SearchResults) PrintDirCounts() {
	countMap := srs.Stats.DirCounts
	countKeys := getSortedCountKeys(countMap)
	printCounts("directories", countKeys)
}

func (srs *SearchResults) PrintFileCounts() {
	countMap := srs.Stats.FileCounts
	countKeys := getSortedCountKeys(countMap)
	printCounts("files", countKeys)
}

func (srs *SearchResults) PrintLineCounts() {
	countMap := srs.Stats.LineCounts
	totalCount := 0
	for _, v := range countMap {
		totalCount += v
	}
	countKeys := getCaseInsensitiveSortedCountKeys(countMap)
	log(fmt.Sprintf("Lines with matches (%d):", totalCount))
	for _, k := range countKeys {
		for i := 0; i < countMap[k]; i++ {
			log(fmt.Sprintf("%s", k))
		}
	}
}

func (srs *SearchResults) PrintUniqueLineCounts() {
	countMap := srs.Stats.LineCounts
	countKeys := getCaseInsensitiveSortedCountKeys(countMap)
	log(fmt.Sprintf("Unique lines with matches (%d):", len(countKeys)))
	for _, k := range countKeys {
		log(fmt.Sprintf("%s", k))
	}
}

func (srs *SearchResults) PrintPatternCounts(patterns []string) {
	printCounts("patterns", patterns)
}

func (srs *SearchResults) PrintSearchResults() {
	// sort them first
	sort.Sort(srs)
	formatter := NewSearchResultFormatter(srs.Settings)
	log(fmt.Sprintf("Search results (%d):", len(srs.SearchResults)))
	for _, r := range srs.SearchResults {
		if len(srs.Stats.PatternCounts) > 1 {
			log(fmt.Sprintf("\"%s\": ", r.Pattern.String()))
		}
		log(formatter.Format(r))
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
	if f.Settings.Colorize {
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

	if formattedLength > f.Settings.MaxLineLength {
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
		for formattedLength < f.Settings.MaxLineLength {
			if lineStartIndex > 0 {
				lineStartIndex -= 1
				matchStartIndex += 1
				matchEndIndex += 1
				formattedLength = lineEndIndex - lineStartIndex
			}
			if formattedLength < f.Settings.MaxLineLength && lineEndIndex < maxLineEndIndex {
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

	if f.Settings.Colorize {
		formatted = colorize(formatted, matchStartIndex, matchEndIndex)
	}
	return formatted
}

type SearchResult struct {
	Pattern         *regexp.Regexp
	File            *SearchItem
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
