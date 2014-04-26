package xsearch

import (
	"bytes"
	"fmt"
	"path/filepath"
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
	rs.DirCounts[filepath.Dir(r.Filepath)]++
	rs.FileCounts[r.Filepath]++
	if strings.TrimSpace(r.Line) != "" {
		rs.LineCounts[strings.TrimSpace(r.Line)]++
	}
	rs.PatternCounts[r.Pattern.String()]++
}

func (rs *SearchResults) HasSearchResults() bool {
	return len(rs.SearchResults) > 0
}

func (rs *SearchResults) HasResultForFileAndPattern(filepath string,
	pattern *regexp.Regexp) bool {
	for _, r := range rs.SearchResults {
		if pattern.String() == r.Pattern.String() && filepath == r.Filepath {
			return true
		}
	}
	return false
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
	fmt.Printf("%s match counts (%d %s):\n", strings.Title(singName),
		len(countKeys), countName)
	longestKeyLen := getLongestLen(countKeys) + 1
	longestNumLen := getNumLen2(getHighestMapVal(countMap))
	lineFormat := fmt.Sprintf("%%-%ds %%%dd\n", longestKeyLen, longestNumLen)
	for _, k := range countKeys {
		fmt.Printf(lineFormat, k+":", countMap[k])
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
	fmt.Printf("Search results (%d):\n", len(rs.SearchResults))
	for _, r := range rs.SearchResults {
		if len(rs.PatternCounts) > 1 {
			fmt.Printf("\"%s\": ", r.Pattern.String())
		}
		fmt.Println(r.String())
	}
}

type SearchResult struct {
	Pattern     *regexp.Regexp
	Filepath    string
	Linenum     int
	Line        string
	LinesBefore []string
	LinesAfter  []string
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
	return len(fmt.Sprintf("%d", r.Linenum+len(r.LinesAfter)))
}

func (r *SearchResult) multiLineString() string {
	var buffer bytes.Buffer
	buffer.WriteString(strings.Repeat("=", SEPARATOR_LEN))
	buffer.WriteString(fmt.Sprintf("\n%s\n", r.Filepath))
	buffer.WriteString(strings.Repeat("-", SEPARATOR_LEN))
	buffer.WriteString("\n")
	lineFormat := fmt.Sprintf(" %%%dd | %%s\n", r.lineNumPadding())
	currentLineNum := r.Linenum
	if len(r.LinesBefore) > 0 {
		currentLineNum -= len(r.LinesBefore)
		for _, l := range r.LinesBefore {
			buffer.WriteString(" " + fmt.Sprintf(lineFormat, currentLineNum, l))
			currentLineNum++
		}
	}
	buffer.WriteString(">" + fmt.Sprintf(lineFormat, currentLineNum, r.Line))
	if len(r.LinesAfter) > 0 {
		currentLineNum++
		for _, l := range r.LinesAfter {
			buffer.WriteString(" " + fmt.Sprintf(lineFormat, currentLineNum, l))
			currentLineNum++
		}
	}
	return buffer.String()
}

func (r *SearchResult) singleLineString() string {
	if r.Linenum > 0 {
		return fmt.Sprintf("%s: %d: %s", r.Filepath, r.Linenum,
			strings.TrimSpace(r.Line))
	} else {
		return fmt.Sprintf("%s matches", r.Filepath)
	}
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
