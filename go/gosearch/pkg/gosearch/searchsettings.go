package gosearch

import (
	"fmt"
	"strings"
)

type SearchSettings struct {
	ArchivesOnly            bool
	Colorize                bool
	Debug                   bool
	ExcludeHidden           bool
	FirstMatch              bool
	InArchiveExtensions     []string
	InArchiveFilePatterns   *SearchPatterns
	InDirPatterns           *SearchPatterns
	InExtensions            []string
	InFilePatterns          *SearchPatterns
	InFileTypes             []FileType
	InLinesAfterPatterns    *SearchPatterns
	InLinesBeforePatterns   *SearchPatterns
	LinesAfter              int
	LinesAfterToPatterns    *SearchPatterns
	LinesAfterUntilPatterns *SearchPatterns
	LinesBefore             int
	ListDirs                bool
	ListFiles               bool
	ListLines               bool
	MaxLineLength           int
	MultiLineSearch         bool
	OutArchiveExtensions    []string
	OutArchiveFilePatterns  *SearchPatterns
	OutDirPatterns          *SearchPatterns
	OutExtensions           []string
	OutFilePatterns         *SearchPatterns
	OutFileTypes            []FileType
	OutLinesAfterPatterns   *SearchPatterns
	OutLinesBeforePatterns  *SearchPatterns
	Paths                   []string
	PrintResults            bool
	PrintUsage              bool
	PrintVersion            bool
	Recursive               bool
	SearchArchives          bool
	SearchPatterns          *SearchPatterns
	TextFileEncoding        string
	UniqueLines             bool
	Verbose                 bool
}

func GetDefaultSearchSettings() *SearchSettings {
	return &SearchSettings{
		false,               // ArchivesOnly
		true,                // Colorize
		false,               // Debug
		true,                // ExcludeHidden
		false,               // FirstMatch
		[]string{},          // InArchiveExtensions
		NewSearchPatterns(), // InArchiveFilePatterns
		NewSearchPatterns(), // InDirPatterns
		[]string{},          // InExtensions
		NewSearchPatterns(), // InFilePatterns
		[]FileType{},        // InFileTypes
		NewSearchPatterns(), // InLinesAfterPatterns
		NewSearchPatterns(), // InLinesBeforePatterns
		0,                   // LinesAfter
		NewSearchPatterns(), // LinesAfterToPatterns
		NewSearchPatterns(), // LinesAfterUntilPatterns
		0,                   // LinesBefore
		false,               // ListDirs
		false,               // ListFiles
		false,               // ListLines
		150,                 // MaxLineLength
		false,               // MultiLineSearch
		[]string{},          // OutArchiveExtensions
		NewSearchPatterns(), // OutArchiveFilePatterns
		NewSearchPatterns(), // OutDirPatterns
		[]string{},          // OutExtensions
		NewSearchPatterns(), // OutFilePatterns
		[]FileType{},        // OutFileTypes
		NewSearchPatterns(), // OutLinesAfterPatterns
		NewSearchPatterns(), // OutLinesBeforePatterns
		[]string{},          // Paths
		true,                // PrintResults
		false,               // PrintUsage
		false,               // PrintVersion
		true,                // Recursive
		false,               // SearchArchives
		NewSearchPatterns(), // SearchPatterns
		"utf-8",             // TextFileEncoding
		false,               // UniqueLines
		false,               // Verbose
	}
}

func (s *SearchSettings) AddInExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			s.InExtensions = append(s.InExtensions, ext)
		}
	}
}

func (s *SearchSettings) AddOutExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		if x != "" {
			ext := strings.ToLower(x)
			s.OutExtensions = append(s.OutExtensions, ext)
		}
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

// func (s *SearchSettings) AddInFileType(t gofind.FileType) {
func (s *SearchSettings) AddInFileType(t FileType) {
	s.InFileTypes = append(s.InFileTypes, t)
}

// func (s *SearchSettings) AddOutFileType(t gofind.FileType) {
func (s *SearchSettings) AddOutFileType(t FileType) {
	s.OutFileTypes = append(s.OutFileTypes, t)
}

func (s *SearchSettings) AddInArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		s.InArchiveExtensions = append(s.InArchiveExtensions, ext)
	}
}

func (s *SearchSettings) AddOutArchiveExtension(xs string) {
	for _, x := range strings.Split(xs, ",") {
		ext := strings.ToLower(x)
		s.OutArchiveExtensions = append(s.OutArchiveExtensions, ext)
	}
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

func (s *SearchSettings) AddPath(p string) {
	s.Paths = append(s.Paths, p)
}

func (s *SearchSettings) AddSearchPattern(p string) {
	addPattern(p, s.SearchPatterns)
}

func (s *SearchSettings) SetArchivesOnly(archivesOnly bool) {
	s.ArchivesOnly = archivesOnly
	if archivesOnly {
		s.SearchArchives = true
	}
}

func (s *SearchSettings) SetDebug(debug bool) {
	s.Debug = debug
	if debug {
		s.Verbose = true
	}
}

func (s *SearchSettings) String() string {
	const template = "SearchSettings{" +
		"ArchivesOnly: %t" +
		", Colorize: %t" +
		", Debug: %t" +
		", ExcludeHidden: %t" +
		", FirstMatch: %t" +
		", InArchiveExtensions: %s" +
		", InArchiveFilePatterns: %s" +
		", InDirPatterns: %s" +
		", InExtensions: %s" +
		", InFilePatterns: %s" +
		", InFileTypes: %s" +
		", InLinesAfterPatterns: %s" +
		", InLinesBeforePatterns: %s" +
		", LinesAfter: %d" +
		", LinesAfterToPatterns: %s" +
		", LinesAfterUntilPatterns: %s" +
		", LinesBefore: %d" +
		", ListDirs: %t" +
		", ListFiles: %t" +
		", ListLines: %t" +
		", MaxLineLength: %d" +
		", MultiLineSearch: %t" +
		", OutArchiveExtensions: %s" +
		", OutArchiveFilePatterns: %s" +
		", OutDirPatterns: %s" +
		", OutExtensions: %s" +
		", OutFilePatterns: %s" +
		", OutFileTypes: %s" +
		", OutLinesAfterPatterns: %s" +
		", OutLinesBeforePatterns: %s" +
		", Paths: %s" +
		", PrintResults: %t" +
		", PrintUsage: %t" +
		", PrintVersion: %t" +
		", Recursive: %t" +
		", SearchArchives: %t" +
		", SearchPatterns: %s" +
		", TextFileEncoding: \"%s\"" +
		", UniqueLines: %t" +
		", Verbose: %t}"
	return fmt.Sprintf(template,
		s.ArchivesOnly,
		s.Colorize,
		s.Debug,
		s.ExcludeHidden,
		s.FirstMatch,
		stringListToString(s.InArchiveExtensions),
		searchPatternsToString(s.InArchiveFilePatterns),
		searchPatternsToString(s.InDirPatterns),
		stringListToString(s.InExtensions),
		searchPatternsToString(s.InFilePatterns),
		fileTypeListToString(s.InFileTypes),
		searchPatternsToString(s.InLinesAfterPatterns),
		searchPatternsToString(s.InLinesBeforePatterns),
		s.LinesAfter,
		searchPatternsToString(s.LinesAfterToPatterns),
		searchPatternsToString(s.LinesAfterUntilPatterns),
		s.LinesBefore,
		s.ListDirs,
		s.ListFiles,
		s.ListLines,
		s.MaxLineLength,
		s.MultiLineSearch,
		stringListToString(s.OutArchiveExtensions),
		searchPatternsToString(s.OutArchiveFilePatterns),
		searchPatternsToString(s.OutDirPatterns),
		stringListToString(s.OutExtensions),
		searchPatternsToString(s.OutFilePatterns),
		fileTypeListToString(s.OutFileTypes),
		searchPatternsToString(s.OutLinesAfterPatterns),
		searchPatternsToString(s.OutLinesBeforePatterns),
		stringListToString(s.Paths),
		s.PrintResults,
		s.PrintUsage,
		s.PrintVersion,
		s.Recursive,
		s.SearchArchives,
		searchPatternsToString(s.SearchPatterns),
		s.TextFileEncoding,
		s.UniqueLines,
		s.Verbose,
	)
}
