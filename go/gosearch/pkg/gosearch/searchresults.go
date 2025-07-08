package gosearch

import (
	"bytes"
	"fmt"
	"gofind/pkg/gofind"
	"regexp"
	"slices"
	"strings"
	"unicode"
)

type SearchResults struct {
	Settings      *SearchSettings
	FileResults   *gofind.FileResults
	SearchResults []*SearchResult
}

func NewSearchResults(settings *SearchSettings) *SearchResults {
	return &SearchResults{
		settings,
		gofind.NewFileResults(),
		[]*SearchResult{},
	}
}

func (srs *SearchResults) AddSearchResult(r *SearchResult) {
	srs.SearchResults = append(srs.SearchResults, r)
	//srs.AddSearchResultStats(r)
}

func (srs *SearchResults) Clear() {
	srs.FileResults = nil
	srs.SearchResults = nil
}

func (srs *SearchResults) IsEmpty() bool {
	return len(srs.SearchResults) == 0
}

// methods for the sort.Interface interface (so you can call sort.Sort on
// SearchResults instances)
func (srs *SearchResults) Len() int {
	return len(srs.SearchResults)
}

func (srs *SearchResults) Sort(settings *SearchSettings) {
	searchResultSorter := NewSearchResultSorter(settings)
	searchResultSorter.Sort(srs.SearchResults)
}

func (srs *SearchResults) PrintMatchingDirs(formatter *SearchResultFormatter) {
	srs.FileResults.PrintMatchingDirs(formatter.FileFormatter)
}

func (srs *SearchResults) PrintMatchingFiles(formatter *SearchResultFormatter) {
	srs.FileResults.PrintMatchingFiles(formatter.FileFormatter)
}

func (srs *SearchResults) getMatchingLineMap() map[string]int {
	lineMap := make(map[string]int)
	for _, r := range srs.SearchResults {
		if strings.TrimSpace(r.Line) != "" {
			lineMap[strings.TrimSpace(r.Line)]++
		}
	}
	return lineMap
}

func (srs *SearchResults) PrintMatchingLines(formatter *SearchResultFormatter) {
	lineMap := srs.getMatchingLineMap()
	totalCount := 0
	for _, v := range lineMap {
		totalCount += v
	}
	countKeys := gofind.GetCaseInsensitiveSortedCountKeys(lineMap)

	if srs.Settings.UniqueLines() {
		gofind.Log(fmt.Sprintf("\nUnique lines with matches (%d):", len(countKeys)))
		for _, k := range countKeys {
			gofind.Log(fmt.Sprintf("%s", formatter.FormatLine(k)))
		}
	} else {
		gofind.Log(fmt.Sprintf("\nLines with matches (%d):", totalCount))
		for _, k := range countKeys {
			for i := 0; i < lineMap[k]; i++ {
				gofind.Log(fmt.Sprintf("%s", formatter.FormatLine(k)))
			}
		}
	}
}

func (srs *SearchResults) PrintSearchResults(formatter *SearchResultFormatter) {
	gofind.Log(fmt.Sprintf("Search results (%d):", srs.Len()))
	for _, r := range srs.SearchResults {
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

type SearchResultSorter struct {
	Settings         *SearchSettings
	FileResultSorter *gofind.FileResultSorter
}

func NewSearchResultSorter(settings *SearchSettings) *SearchResultSorter {
	f := &SearchResultSorter{
		settings,
		gofind.NewFileResultSorter(settings.FindSettings),
	}
	return f
}

func (srs *SearchResultSorter) compareBySearchFields(sr1, sr2 *SearchResult) int {
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
		if sr1.MatchEndIndex < sr2.MatchEndIndex {
			return -1
		}
		return 1
	}
	if sr1.LineNum < sr2.LineNum {
		return -1
	}
	return 1
}

func (srs *SearchResultSorter) CompareByPath(sr1, sr2 *SearchResult) int {
	cmp := srs.FileResultSorter.CompareByPath(sr1.File, sr2.File)
	if cmp == 0 {
		return srs.compareBySearchFields(sr1, sr2)
	}
	return cmp
}

func (srs *SearchResultSorter) getCompareByPath() func(sr1, sr2 *SearchResult) int {
	if srs.Settings.FindSettings.SortDescending() {
		return func(sr1, sr2 *SearchResult) int {
			return srs.CompareByPath(sr2, sr1)
		}
	}
	return func(sr1, sr2 *SearchResult) int {
		return srs.CompareByPath(sr1, sr2)
	}
}

func (srs *SearchResultSorter) CompareByName(sr1, sr2 *SearchResult) int {
	cmp := srs.FileResultSorter.CompareByName(sr1.File, sr2.File)
	if cmp == 0 {
		return srs.compareBySearchFields(sr1, sr2)
	}
	return cmp
}

func (srs *SearchResultSorter) getCompareByName() func(sr1, sr2 *SearchResult) int {
	if srs.Settings.FindSettings.SortDescending() {
		return func(sr1, sr2 *SearchResult) int {
			return srs.CompareByName(sr2, sr1)
		}
	}
	return func(sr1, sr2 *SearchResult) int {
		return srs.CompareByName(sr1, sr2)
	}
}

func (srs *SearchResultSorter) CompareBySize(sr1, sr2 *SearchResult) int {
	cmp := srs.FileResultSorter.CompareBySize(sr1.File, sr2.File)
	if cmp == 0 {
		return srs.compareBySearchFields(sr1, sr2)
	}
	return cmp
}

func (srs *SearchResultSorter) getCompareBySize() func(sr1, sr2 *SearchResult) int {
	if srs.Settings.FindSettings.SortDescending() {
		return func(sr1, sr2 *SearchResult) int {
			return srs.CompareBySize(sr2, sr1)
		}
	}
	return func(sr1, sr2 *SearchResult) int {
		return srs.CompareBySize(sr1, sr2)
	}
}

func (srs *SearchResultSorter) CompareByType(sr1, sr2 *SearchResult) int {
	cmp := srs.FileResultSorter.CompareByType(sr1.File, sr2.File)
	if cmp == 0 {
		return srs.compareBySearchFields(sr1, sr2)
	}
	return cmp
}

func (srs *SearchResultSorter) getCompareByType() func(sr1, sr2 *SearchResult) int {
	if srs.Settings.FindSettings.SortDescending() {
		return func(sr1, sr2 *SearchResult) int {
			return srs.CompareByType(sr2, sr1)
		}
	}
	return func(sr1, sr2 *SearchResult) int {
		return srs.CompareByType(sr1, sr2)
	}
}

func (srs *SearchResultSorter) CompareByLastMod(sr1, sr2 *SearchResult) int {
	cmp := srs.FileResultSorter.CompareByType(sr1.File, sr2.File)
	if cmp == 0 {
		return srs.compareBySearchFields(sr1, sr2)
	}
	return cmp
}

func (srs *SearchResultSorter) getCompareByLastMod() func(sr1, sr2 *SearchResult) int {
	if srs.Settings.FindSettings.SortDescending() {
		return func(sr1, sr2 *SearchResult) int {
			return srs.CompareByLastMod(sr2, sr1)
		}
	}
	return func(sr1, sr2 *SearchResult) int {
		return srs.CompareByLastMod(sr1, sr2)
	}
}

func (srs *SearchResultSorter) getSortFunc() func(sr1, sr2 *SearchResult) int {
	settings := srs.Settings
	switch settings.SortBy() {
	case gofind.SortByFileName:
		return srs.getCompareByName()
	case gofind.SortByFileSize:
		return srs.getCompareBySize()
	case gofind.SortByFileType:
		return srs.getCompareByType()
	case gofind.SortByLastMod:
		return srs.getCompareByLastMod()
	default:
		return srs.getCompareByPath()
	}
}

func (srs *SearchResultSorter) Sort(searchResults []*SearchResult) {
	sortFunc := srs.getSortFunc()
	slices.SortFunc(searchResults, sortFunc)
}
