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

func (srs *SearchResults) getSortByPath(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return srs.CompareByPath(srs.SearchResults[j], srs.SearchResults[i], sortCaseInsensitive) < 0
		}
	}
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

func (srs *SearchResults) getSortByName(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return srs.CompareByName(srs.SearchResults[j], srs.SearchResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return srs.CompareByName(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) CompareBySize(sr1, sr2 *SearchResult, sortCaseInsensitive bool) int {
	sres := srs.FileResults.CompareBySize(sr1.File, sr2.File, sortCaseInsensitive)
	if sres == 0 {
		return compareSearchFields(sr1, sr2)
	}
	if sres < 0 {
		return -1
	}
	return 1
}

func (srs *SearchResults) getSortBySize(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return srs.CompareBySize(srs.SearchResults[j], srs.SearchResults[i], sortCaseInsensitive) < 0
		}
	}
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

func (srs *SearchResults) getSortByType(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return srs.CompareByType(srs.SearchResults[j], srs.SearchResults[i], sortCaseInsensitive) < 0
		}
	}
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

func (srs *SearchResults) getSortByLastMod(sortCaseInsensitive, sortDescending bool) func(i, j int) bool {
	if sortDescending {
		return func(i, j int) bool {
			return srs.CompareByLastMod(srs.SearchResults[j], srs.SearchResults[i], sortCaseInsensitive) < 0
		}
	}
	return func(i, j int) bool {
		return srs.CompareByLastMod(srs.SearchResults[i], srs.SearchResults[j], sortCaseInsensitive) < 0
	}
}

func (srs *SearchResults) getSortComparator(settings *SearchSettings) func(i, j int) bool {
	switch settings.SortBy() {
	case gofind.SortByFileName:
		return srs.getSortByName(settings.SortCaseInsensitive(), settings.SortDescending())
	case gofind.SortByFileSize:
		return srs.getSortBySize(settings.SortCaseInsensitive(), settings.SortDescending())
	case gofind.SortByFileType:
		return srs.getSortByType(settings.SortCaseInsensitive(), settings.SortDescending())
	case gofind.SortByLastMod:
		return srs.getSortByLastMod(settings.SortCaseInsensitive(), settings.SortDescending())
	default:
		return srs.getSortByPath(settings.SortCaseInsensitive(), settings.SortDescending())
	}
}

func (srs *SearchResults) Sort(settings *SearchSettings) {
	sortComparator := srs.getSortComparator(settings)
	sort.Slice(srs.SearchResults, sortComparator)
}

func (srs *SearchResults) PrintMatchingDirs(formatter *SearchResultFormatter) {
	srs.FileResults.PrintMatchingDirs(formatter.FileFormatter)
}

func (srs *SearchResults) PrintMatchingFiles(formatter *SearchResultFormatter) {
	srs.FileResults.PrintMatchingFiles(formatter.FileFormatter)
}

func (srs *SearchResults) PrintMatchingLines(formatter *SearchResultFormatter) {
	countMap := srs.Stats.LineCounts
	totalCount := 0
	for _, v := range countMap {
		totalCount += v
	}
	countKeys := gofind.GetCaseInsensitiveSortedCountKeys(countMap)

	if srs.Settings.UniqueLines() {
		gofind.Log(fmt.Sprintf("\nUnique lines with matches (%d):", len(countKeys)))
		for _, k := range countKeys {
			gofind.Log(fmt.Sprintf("%s", formatter.FormatLine(k)))
		}
	} else {
		gofind.Log(fmt.Sprintf("\nLines with matches (%d):", totalCount))
		for _, k := range countKeys {
			for i := 0; i < countMap[k]; i++ {
				gofind.Log(fmt.Sprintf("%s", formatter.FormatLine(k)))
			}
		}
	}
}

func (srs *SearchResults) PrintSearchResults(formatter *SearchResultFormatter) {
	gofind.Log(fmt.Sprintf("Search results (%d):", srs.Len()))
	for _, r := range srs.SearchResults {
		if len(srs.Stats.PatternCounts) > 1 {
			gofind.Log(fmt.Sprintf("\"%s\": ", r.Pattern.String()))
		}
		gofind.Log(formatter.Format(r))
	}
}

type SearchResultFormatter struct {
	Settings      *SearchSettings
	FileFormatter *gofind.FileResultFormatter
	FormatLine    gofind.StringFormatter
}

func NewSearchResultFormatter(settings *SearchSettings) *SearchResultFormatter {
	fileFormatter := gofind.NewFileResultFormatter(settings.FindSettings)
	formatLine := func(line string) string { return line }
	f := &SearchResultFormatter{
		settings,
		fileFormatter,
		formatLine,
	}
	if settings.Colorize() {
		formatLine = func(line string) string { return f.formatLineWithColor(line) }
		f = &SearchResultFormatter{
			settings,
			fileFormatter,
			formatLine,
		}
	}
	return f
}

func (f *SearchResultFormatter) formatLineWithColor(line string) string {
	formattedLine := line
	it := f.Settings.SearchPatterns().Iterator()
	for it.Next() {
		p := it.Value()
		if match := p.FindStringIndex(formattedLine); match != nil {
			formattedLine = colorize(formattedLine, match[0], match[1])
			break
		}
	}
	return formattedLine
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
	return gofind.Colorize(s, matchStartIndex, matchEndIndex)
}

func (f *SearchResultFormatter) multiLineFormat(r *SearchResult) string {
	var buffer bytes.Buffer
	buffer.WriteString(strings.Repeat("=", SEPARATOR_LEN))
	buffer.WriteString(fmt.Sprintf("\n%s: %d: [%d:%d]\n",
		f.FileFormatter.FormatFileResult(r.File), r.LineNum, r.MatchStartIndex, r.MatchEndIndex))
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
		return fmt.Sprintf("%s: %d: [%d:%d]: %s", f.FileFormatter.FormatFileResult(r.File),
			r.LineNum, r.MatchStartIndex, r.MatchEndIndex, f.formatMatchingLine(r))
	} else {
		return fmt.Sprintf("%s matches at [%d:%d]", f.FileFormatter.FormatFileResult(r.File),
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
