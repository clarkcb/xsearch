package xsearch

import (
	"bytes"
	"fmt"
	"regexp"
	"strings"
)

type SearchSettings struct {
	StartPath               string
	InExtensions            []string
	OutExtensions           []string
	InDirPatterns           []*regexp.Regexp
	OutDirPatterns          []*regexp.Regexp
	InFilePatterns          []*regexp.Regexp
	OutFilePatterns         []*regexp.Regexp
	InLinesAfterPatterns    []*regexp.Regexp
	OutLinesAfterPatterns   []*regexp.Regexp
	InLinesBeforePatterns   []*regexp.Regexp
	OutLinesBeforePatterns  []*regexp.Regexp
	LinesAfterToPatterns    []*regexp.Regexp
	LinesAfterUntilPatterns []*regexp.Regexp
	SearchPatterns          []*regexp.Regexp
	CaseSensitive           bool
	Debug                   bool
	DoTiming                bool
	FirstMatch              bool
	LinesAfter              int
	LinesBefore             int
	ListDirs                bool
	ListFiles               bool
	ListLines               bool
	MultiLineSearch         bool
	PrintResults            bool
	PrintUsage              bool
	PrintVersion            bool
	SearchCompressed        bool
	UniqueLines             bool
	Verbose                 bool
}

func GetDefaultSearchSettings() *SearchSettings {
	DEFAULTOUTDIRPATTERNS := []*regexp.Regexp{
		regexp.MustCompile("\\.git\\b"),
		regexp.MustCompile("\\.svn\\b"),
		regexp.MustCompile("\\bCVS\\b"),
	}
	DEFAULTOUTFILEPATTERNS := []*regexp.Regexp{
		regexp.MustCompile("\\.DS_Store\\b"),
	}
	return &SearchSettings{
		"",                     // StartPath
		[]string{},             // InExtensions
		[]string{},             // OutExtensions
		[]*regexp.Regexp{},     // InDirPatterns
		DEFAULTOUTDIRPATTERNS,  // OutDirPatterns
		[]*regexp.Regexp{},     // InFilePatterns
		DEFAULTOUTFILEPATTERNS, // OutFilePatterns
		[]*regexp.Regexp{},     // InLinesAfterPatterns
		[]*regexp.Regexp{},     // OutLinesAfterPatterns
		[]*regexp.Regexp{},     // InLinesBeforePatterns
		[]*regexp.Regexp{},     // OutLinesBeforePatterns
		[]*regexp.Regexp{},     // LinesAfterToPatterns
		[]*regexp.Regexp{},     // LinesAfterUntilPatterns
		[]*regexp.Regexp{},     // SearchPatterns
		true,                   // CaseSensitive
		false,                  // Debug
		false,                  // DoTiming
		false,                  // FirstMatch
		0,                      // LinesAfter
		0,                      // LinesBefore
		false,                  // ListDirs
		false,                  // ListFiles
		false,                  // ListLines
		false,                  // MultiLineSearch
		true,                   // PrintResults
		false,                  // PrintUsage
		false,                  // PrintVersion
		false,                  // SearchCompressed
		false,                  // UniqueLines
		false,                  // Verbose
	}
}

func (s *SearchSettings) AddInExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		s.InExtensions = append(s.InExtensions, strings.ToLower(x))
	}
}

func (s *SearchSettings) AddOutExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		s.OutExtensions = append(s.OutExtensions, strings.ToLower(x))
	}
}

func addPattern(p string, patterns *[]*regexp.Regexp) {
	*patterns = append(*patterns, regexp.MustCompile(p))
}

func (s *SearchSettings) AddInDirPattern(p string) {
	addPattern(p, &s.InDirPatterns)
}

func (s *SearchSettings) AddOutDirPattern(p string) {
	addPattern(p, &s.OutDirPatterns)
}

func (s *SearchSettings) AddInFilePattern(p string) {
	addPattern(p, &s.InFilePatterns)
}

func (s *SearchSettings) AddOutFilePattern(p string) {
	addPattern(p, &s.OutFilePatterns)
}

func (s *SearchSettings) AddInLinesBeforePattern(p string) {
	addPattern(p, &s.InLinesBeforePatterns)
}

func (s *SearchSettings) AddOutLinesBeforePattern(p string) {
	addPattern(p, &s.OutLinesBeforePatterns)
}

func (s *SearchSettings) AddInLinesAfterPattern(p string) {
	addPattern(p, &s.InLinesAfterPatterns)
}

func (s *SearchSettings) AddOutLinesAfterPattern(p string) {
	addPattern(p, &s.OutLinesAfterPatterns)
}

func (s *SearchSettings) AddLinesAfterToPattern(p string) {
	addPattern(p, &s.LinesAfterToPatterns)
}

func (s *SearchSettings) AddLinesAfterUntilPattern(p string) {
	addPattern(p, &s.LinesAfterUntilPatterns)
}

func (s *SearchSettings) AddSearchPattern(p string) {
	addPattern(p, &s.SearchPatterns)
}

func addRegexpListToBuffer(name string, list *[]*regexp.Regexp, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	for i, r := range *list {
		if i > 0 {
			buffer.WriteString(",")
		}
		buffer.WriteString(r.String())
	}
	buffer.WriteString("]")
}

func addStringListToBuffer(name string, list *[]string, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	buffer.WriteString(strings.Join(*list, ","))
	buffer.WriteString("]")
}

func (s *SearchSettings) String() string {
	var buffer bytes.Buffer
	buffer.WriteString("SearchSettings{")
	buffer.WriteString(fmt.Sprintf("StartPath: %s", s.StartPath))
	buffer.WriteString(", ")
	addStringListToBuffer("InExtensions", &s.InExtensions, &buffer)
	buffer.WriteString(", ")
	addStringListToBuffer("OutExtensions", &s.OutExtensions, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("InDirPatterns", &s.InDirPatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("OutDirPatterns", &s.OutDirPatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("InFilePatterns", &s.InFilePatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("OutFilePatterns", &s.OutFilePatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("InLinesAfterPatterns", &s.InLinesAfterPatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("OutLinesAfterPatterns", &s.OutLinesAfterPatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("InLinesBeforePatterns", &s.InLinesBeforePatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("OutLinesBeforePatterns", &s.OutLinesBeforePatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("LinesAfterToPatterns", &s.LinesAfterToPatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("LinesAfterUntilPatterns", &s.LinesAfterUntilPatterns, &buffer)
	buffer.WriteString(", ")
	addRegexpListToBuffer("SearchPatterns", &s.SearchPatterns, &buffer)
	buffer.WriteString(fmt.Sprintf(", CaseSensitive: %t", s.CaseSensitive))
	buffer.WriteString(fmt.Sprintf(", Debug: %t", s.Debug))
	buffer.WriteString(fmt.Sprintf(", DoTiming: %t", s.DoTiming))
	buffer.WriteString(fmt.Sprintf(", FirstMatch: %t", s.FirstMatch))
	buffer.WriteString(fmt.Sprintf(", LinesAfter: %d", s.LinesAfter))
	buffer.WriteString(fmt.Sprintf(", LinesBefore: %d", s.LinesBefore))
	buffer.WriteString(fmt.Sprintf(", ListDirs: %t", s.ListDirs))
	buffer.WriteString(fmt.Sprintf(", ListFiles: %t", s.ListFiles))
	buffer.WriteString(fmt.Sprintf(", ListLines: %t", s.ListLines))
	buffer.WriteString(fmt.Sprintf(", MultiLineSearch: %t", s.MultiLineSearch))
	buffer.WriteString(fmt.Sprintf(", PrintResults: %t", s.PrintResults))
	buffer.WriteString(fmt.Sprintf(", PrintUsage: %t", s.PrintUsage))
	buffer.WriteString(fmt.Sprintf(", PrintVersion: %t", s.PrintVersion))
	buffer.WriteString(fmt.Sprintf(", SearchCompressed: %t", s.SearchCompressed))
	buffer.WriteString(fmt.Sprintf(", UniqueLines: %t", s.UniqueLines))
	buffer.WriteString(fmt.Sprintf(", Verbose: %t", s.Verbose))
	buffer.WriteString("}")
	return buffer.String()
}
