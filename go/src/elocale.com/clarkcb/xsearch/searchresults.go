package xsearch

import (
	"bytes"
	"fmt"
	"regexp"
	"sort"
	"strings"
)

type SearchResults struct {
	SearchResults []*SearchResult
	DirCounts     map[string]int
	FileCounts    map[string]int
	LineCounts    map[string]int
	PatternCounts map[string]int
}

func NewSearchResults() *SearchResults {
	return &SearchResults{
		[]*SearchResult{},
		make(map[string]int),
		make(map[string]int),
		make(map[string]int),
		make(map[string]int),
	}
}

func (rs *SearchResults) AddSearchResult(r *SearchResult) {
	rs.SearchResults = append(rs.SearchResults, r)
	rs.DirCounts[*r.File.Path]++
	rs.FileCounts[r.File.String()]++
	if strings.TrimSpace(*r.Line) != "" {
		rs.LineCounts[strings.TrimSpace(*r.Line)]++
	}
	rs.PatternCounts[r.Pattern.String()]++
}

func (rs *SearchResults) Clear() {
	rs.DirCounts = make(map[string]int)
	rs.FileCounts = make(map[string]int)
	rs.LineCounts = make(map[string]int)
	rs.PatternCounts = make(map[string]int)
	rs.SearchResults = nil
}

func (rs *SearchResults) IsEmpty() bool {
	return len(rs.SearchResults) == 0
}

func (rs *SearchResults) HasResultForFileAndPattern(si *SearchItem,
	pattern *regexp.Regexp) bool {
	for _, r := range rs.SearchResults {
		if pattern.String() == r.Pattern.String() && si.String() == r.File.String() {
			return true
		}
	}
	return false
}

// methods for the sort.Interface interface (so you can call sort.Sort on
// SearchResults instances)
func (rs *SearchResults) Len() int {
	return len(rs.SearchResults)
}

func (srs *SearchResults) Less(i, j int) bool {
	sr1, sr2 := srs.SearchResults[i], srs.SearchResults[j]
	fileCmp := bytes.Compare([]byte(sr1.File.String()), []byte(sr2.File.String()))
	if fileCmp == 0 {
		if sr1.LineNum == sr2.LineNum {
			return sr1.MatchStartIndex <= sr2.MatchStartIndex
		} else {
			return sr1.LineNum < sr2.LineNum
		}
	} else if fileCmp == -1 {
		return true
	} else {
		return false
	}
}

func (srs *SearchResults) Swap(i, j int) {
	srs.SearchResults[j], srs.SearchResults[i] = srs.SearchResults[i], srs.SearchResults[j]
}

func getSortedCountKeys(m map[string]int) []string {
	mk := make([]string, len(m))
	i := 0
	for k, _ := range m {
		mk[i] = k
		i++
	}
	sort.Strings(mk)
	return mk
}

func getHighestMapVal(m map[string]int) int {
	highestVal := 0
	for _, v := range m {
		if v > highestVal {
			highestVal = v
		}
	}
	return highestVal
}

func getNumLen(num int) int {
	numStr := fmt.Sprintf("%d", num)
	return len(numStr)
}

// just for fun, a strictly numeric way to get number length
func getNumLen2(num int) int {
	next, mult := 10, 10
	count, maxcount := 1, 10
	for count < maxcount {
		if num < next {
			return count
		}
		next = next * mult
		count++
	}
	return count
}

func printCounts(singName string, pluralName string, countKeys []string,
	countMap map[string]int) {
	countName := pluralName
	if len(countKeys) == 1 {
		countName = singName
	}
	log(fmt.Sprintf("%s match counts (%d %s):\n", strings.Title(singName),
		len(countKeys), countName))
	longestKeyLen := getLongestLen(countKeys) + 1
	longestNumLen := getNumLen2(getHighestMapVal(countMap))
	lineFormat := fmt.Sprintf("%%-%ds %%%dd", longestKeyLen, longestNumLen)
	for _, k := range countKeys {
		log(fmt.Sprintf(lineFormat, k+":", countMap[k]))
	}
}

func (rs *SearchResults) PrintDirCounts() {
	countMap := rs.DirCounts
	countKeys := getSortedCountKeys(countMap)
	printCounts("directory", "directories", countKeys, countMap)
}

func (rs *SearchResults) PrintFileCounts() {
	countMap := rs.FileCounts
	countKeys := getSortedCountKeys(countMap)
	printCounts("file", "files", countKeys, countMap)
}

func (rs *SearchResults) PrintLineCounts() {
	countMap := rs.LineCounts
	countKeys := getSortedCountKeys(countMap)
	printCounts("line", "lines", countKeys, countMap)
}

func (rs *SearchResults) PrintPatternCounts(patterns []string) {
	countMap := rs.PatternCounts
	printCounts("pattern", "patterns", patterns, countMap)
}

func (rs *SearchResults) PrintSearchResults() {
	// sort them first
	sort.Sort(rs)
	log(fmt.Sprintf("Search results (%d):", len(rs.SearchResults)))
	for _, r := range rs.SearchResults {
		if len(rs.PatternCounts) > 1 {
			log(fmt.Sprintf("\"%s\": ", r.Pattern.String()))
		}
		log(r.String())
	}
}

type SearchResult struct {
	Pattern         *regexp.Regexp
	File            *SearchItem
	LineNum         int
	MatchStartIndex int
	MatchEndIndex   int
	Line            *string
	LinesBefore     []*string
	LinesAfter      []*string
}

func (r *SearchResult) String() string {
	if len(r.LinesBefore) > 0 || len(r.LinesAfter) > 0 {
		return r.multiLineString()
	} else {
		return r.singleLineString()
	}
}

const SEPARATOR_LEN = 80

func (r *SearchResult) lineNumPadding() int {
	return len(fmt.Sprintf("%d", r.LineNum+len(r.LinesAfter)))
}

func (r *SearchResult) multiLineString() string {
	var buffer bytes.Buffer
	buffer.WriteString(strings.Repeat("=", SEPARATOR_LEN))
	buffer.WriteString(fmt.Sprintf("\n%s\n", r.File.String()))
	buffer.WriteString(strings.Repeat("-", SEPARATOR_LEN))
	buffer.WriteString("\n")
	lineFormat := fmt.Sprintf(" %%%dd | %%s\n", r.lineNumPadding())
	currentLineNum := r.LineNum
	if len(r.LinesBefore) > 0 {
		currentLineNum -= len(r.LinesBefore)
		for _, l := range r.LinesBefore {
			buffer.WriteString(" " + fmt.Sprintf(lineFormat, currentLineNum, *l))
			currentLineNum++
		}
	}
	buffer.WriteString(">" + fmt.Sprintf(lineFormat, currentLineNum, *r.Line))
	if len(r.LinesAfter) > 0 {
		currentLineNum++
		for _, l := range r.LinesAfter {
			buffer.WriteString(" " + fmt.Sprintf(lineFormat, currentLineNum, *l))
			currentLineNum++
		}
	}
	return buffer.String()
}

func (r *SearchResult) singleLineString() string {
	if r.LineNum > 0 {
		return fmt.Sprintf("%s: %d: [%d:%d]: %s", r.File.String(), r.LineNum,
			r.MatchStartIndex, r.MatchEndIndex, r.formatMatchingLine())
	} else {
		return fmt.Sprintf("%s matches", r.File.String())
	}
}

// temp
const MAXLINELENGTH = 150

// TODO: resize line as necessary to match MaxLineLength
func (r *SearchResult) formatMatchingLine() string {
	formatted := *r.Line
	lineLength := len(formatted)
	matchLength := r.MatchEndIndex - r.MatchStartIndex

	if lineLength > MAXLINELENGTH {
		adjustedMaxLength := MAXLINELENGTH - matchLength
		beforeIndex := r.MatchStartIndex
		if r.MatchStartIndex > 0 {
			beforeIndex = beforeIndex - (adjustedMaxLength / 4)
			if beforeIndex < 0 {
				beforeIndex = 0
			}
		}
		adjustedMaxLength = adjustedMaxLength - (r.MatchStartIndex - beforeIndex)
		afterIndex := r.MatchEndIndex + adjustedMaxLength
		if afterIndex > lineLength {
			afterIndex = lineLength
		}

		before := ""
		if beforeIndex > 3 {
			before = "..."
			beforeIndex += 3
		}
		after := ""
		if afterIndex < lineLength-3 {
			after = "..."
			afterIndex -= 3
		}
		formatted = before + formatted[beforeIndex:afterIndex] + after
	}
	return strings.TrimSpace(formatted)
}

func (r *SearchResult) Text() string {
	var buffer bytes.Buffer
	for _, l := range r.LinesBefore {
		buffer.WriteString(fmt.Sprintf("%s\n", *l))
	}
	buffer.WriteString(fmt.Sprintf("%s\n", *r.Line))
	for _, l := range r.LinesAfter {
		buffer.WriteString(fmt.Sprintf("%s\n", *l))
	}
	return buffer.String()
}
