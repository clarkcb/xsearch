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
	InDirPatterns           *SearchPatterns
	OutDirPatterns          *SearchPatterns
	InFilePatterns          *SearchPatterns
	OutFilePatterns         *SearchPatterns
	InArchiveFilePatterns   *SearchPatterns
	OutArchiveFilePatterns  *SearchPatterns
	InLinesAfterPatterns    *SearchPatterns
	OutLinesAfterPatterns   *SearchPatterns
	InLinesBeforePatterns   *SearchPatterns
	OutLinesBeforePatterns  *SearchPatterns
	LinesAfterToPatterns    *SearchPatterns
	LinesAfterUntilPatterns *SearchPatterns
	SearchPatterns          *SearchPatterns
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
	SearchArchives          bool
	UniqueLines             bool
	Verbose                 bool
}

func GetDefaultOutDirPatterns() *SearchPatterns {
	return &SearchPatterns{
		[]*regexp.Regexp{
			regexp.MustCompile("\\.git\\b"),
			regexp.MustCompile("\\.svn\\b"),
			regexp.MustCompile("\\bCVS\\b"),
		},
	}
}

func GetDefaultOutFilePatterns() *SearchPatterns {
	return &SearchPatterns{
		[]*regexp.Regexp{
			regexp.MustCompile("\\.DS_Store\\b"),
		},
	}
}

func GetDefaultSearchSettings() *SearchSettings {
	return &SearchSettings{
		"",                          // StartPath
		[]string{},                  // InExtensions
		[]string{},                  // OutExtensions
		NewSearchPatterns(),         // InDirPatterns
		GetDefaultOutDirPatterns(),  // OutDirPatterns
		NewSearchPatterns(),         // InFilePatterns
		GetDefaultOutFilePatterns(), // OutFilePatterns
		NewSearchPatterns(),         // InArchiveFilePatterns
		NewSearchPatterns(),         // OutArchiveFilePatterns
		NewSearchPatterns(),         // InLinesAfterPatterns
		NewSearchPatterns(),         // OutLinesAfterPatterns
		NewSearchPatterns(),         // InLinesBeforePatterns
		NewSearchPatterns(),         // OutLinesBeforePatterns
		NewSearchPatterns(),         // LinesAfterToPatterns
		NewSearchPatterns(),         // LinesAfterUntilPatterns
		NewSearchPatterns(),         // SearchPatterns
		true,                        // CaseSensitive
		false,                       // Debug
		false,                       // DoTiming
		false,                       // FirstMatch
		0,                           // LinesAfter
		0,                           // LinesBefore
		false,                       // ListDirs
		false,                       // ListFiles
		false,                       // ListLines
		false,                       // MultiLineSearch
		true,                        // PrintResults
		false,                       // PrintUsage
		false,                       // PrintVersion
		false,                       // SearchArchives
		false,                       // UniqueLines
		false,                       // Verbose
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

func addPattern(p string, sp *SearchPatterns) {
	sp.AddPattern(p)
}

func (s *SearchSettings) AddInDirPattern(p string) {
	addPattern(p, s.InDirPatterns)
}

func (s *SearchSettings) AddOutDirPattern(p string) {
	addPattern(p, s.OutDirPatterns)
}

func (s *SearchSettings) AddInFilePattern(p string) {
	addPattern(p, s.InFilePatterns)
}

func (s *SearchSettings) AddOutFilePattern(p string) {
	addPattern(p, s.OutFilePatterns)
}

func (s *SearchSettings) AddInArchiveFilePattern(p string) {
	addPattern(p, s.InArchiveFilePatterns)
}

func (s *SearchSettings) AddOutArchiveFilePattern(p string) {
	addPattern(p, s.OutArchiveFilePatterns)
}

func (s *SearchSettings) AddInLinesBeforePattern(p string) {
	addPattern(p, s.InLinesBeforePatterns)
}

func (s *SearchSettings) AddOutLinesBeforePattern(p string) {
	addPattern(p, s.OutLinesBeforePatterns)
}

func (s *SearchSettings) AddInLinesAfterPattern(p string) {
	addPattern(p, s.InLinesAfterPatterns)
}

func (s *SearchSettings) AddOutLinesAfterPattern(p string) {
	addPattern(p, s.OutLinesAfterPatterns)
}

func (s *SearchSettings) AddLinesAfterToPattern(p string) {
	addPattern(p, s.LinesAfterToPatterns)
}

func (s *SearchSettings) AddLinesAfterUntilPattern(p string) {
	addPattern(p, s.LinesAfterUntilPatterns)
}

func (s *SearchSettings) AddSearchPattern(p string) {
	addPattern(p, s.SearchPatterns)
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

func addSearchPatternsToBuffer(name string, sp *SearchPatterns, buffer *bytes.Buffer) {
	buffer.WriteString(fmt.Sprintf("%s: [", name))
	for i, r := range sp.patterns {
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
	addSearchPatternsToBuffer("InDirPatterns", s.InDirPatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("OutDirPatterns", s.OutDirPatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("InFilePatterns", s.InFilePatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("OutFilePatterns", s.OutFilePatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("InArchiveFilePatterns", s.InArchiveFilePatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("OutArchiveFilePatterns", s.OutArchiveFilePatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("InLinesAfterPatterns", s.InLinesAfterPatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("OutLinesAfterPatterns", s.OutLinesAfterPatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("InLinesBeforePatterns", s.InLinesBeforePatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("OutLinesBeforePatterns", s.OutLinesBeforePatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("LinesAfterToPatterns", s.LinesAfterToPatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("LinesAfterUntilPatterns", s.LinesAfterUntilPatterns, &buffer)
	buffer.WriteString(", ")
	addSearchPatternsToBuffer("SearchPatterns", s.SearchPatterns, &buffer)
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
	buffer.WriteString(fmt.Sprintf(", SearchArchives: %t", s.SearchArchives))
	buffer.WriteString(fmt.Sprintf(", UniqueLines: %t", s.UniqueLines))
	buffer.WriteString(fmt.Sprintf(", Verbose: %t", s.Verbose))
	buffer.WriteString("}")
	return buffer.String()
}
