package gosearch

import (
	"fmt"
	"gofind/pkg/gofind"
	"golang.org/x/text/encoding/ianaindex"
	"time"
)

type SearchSettings struct {
	FindSettings            *gofind.FindSettings
	colorize                bool
	firstMatch              bool
	inLinesAfterPatterns    *gofind.Patterns
	inLinesBeforePatterns   *gofind.Patterns
	linesAfter              int
	linesAfterToPatterns    *gofind.Patterns
	linesAfterUntilPatterns *gofind.Patterns
	linesBefore             int
	listLines               bool
	maxLineLength           int
	multiLineSearch         bool
	outLinesAfterPatterns   *gofind.Patterns
	outLinesBeforePatterns  *gofind.Patterns
	printResults            bool
	searchArchives          bool
	searchPatterns          *gofind.Patterns
	textFileEncoding        string
	uniqueLines             bool
}

func GetDefaultSearchSettings() *SearchSettings {
	return &SearchSettings{
		gofind.GetDefaultFindSettings(),
		true,                 // Colorize
		false,                // FirstMatch
		gofind.NewPatterns(), // InLinesAfterPatterns
		gofind.NewPatterns(), // InLinesBeforePatterns
		0,                    // LinesAfter
		gofind.NewPatterns(), // LinesAfterToPatterns
		gofind.NewPatterns(), // LinesAfterUntilPatterns
		0,                    // LinesBefore
		false,                // ListLines
		150,                  // MaxLineLength
		false,                // MultiLineSearch
		gofind.NewPatterns(), // OutLinesAfterPatterns
		gofind.NewPatterns(), // OutLinesBeforePatterns
		true,                 // PrintResults
		false,                // SearchArchives
		gofind.NewPatterns(), // SearchPatterns
		"utf-8",              // TextFileEncoding
		false,                // UniqueLines
	}
}

func (s *SearchSettings) Validate() error {
	err := s.FindSettings.Validate()
	if err != nil {
		return err
	}
	if s.SearchPatterns().IsEmpty() {
		return fmt.Errorf("No search patterns defined")
	}
	if s.LinesAfter() < 0 {
		return fmt.Errorf("Invalid linesafter")
	}
	if s.LinesBefore() < 0 {
		return fmt.Errorf("Invalid linesbefore")
	}
	if s.MaxLineLength() < 0 {
		return fmt.Errorf("Invalid maxlinelength")
	}
	enc, err := ianaindex.IANA.Encoding(s.TextFileEncoding())
	if err != nil && enc == nil {
		return fmt.Errorf("Invalid or unsupported text file encoding")
	}
	return nil
}

func (s *SearchSettings) ArchivesOnly() bool {
	return s.FindSettings.ArchivesOnly()
}

func (s *SearchSettings) SetArchivesOnly(archivesOnly bool) {
	s.FindSettings.SetArchivesOnly(archivesOnly)
	if archivesOnly {
		s.searchArchives = true
	}
}

func (s *SearchSettings) Colorize() bool {
	return s.colorize
}

func (s *SearchSettings) SetColorize(b bool) {
	s.colorize = b
}

func (s *SearchSettings) Debug() bool {
	return s.FindSettings.Debug()
}

func (s *SearchSettings) SetDebug(b bool) {
	s.FindSettings.SetDebug(b)
}

func (s *SearchSettings) ExcludeHidden() bool {
	return s.FindSettings.ExcludeHidden()
}

func (s *SearchSettings) SetExcludeHidden(b bool) {
	s.FindSettings.SetExcludeHidden(b)
}

func (s *SearchSettings) FirstMatch() bool {
	return s.firstMatch
}

func (s *SearchSettings) SetFirstMatch(b bool) {
	s.firstMatch = b
}

func (s *SearchSettings) InArchiveExtensions() []string {
	return s.FindSettings.InArchiveExtensions()
}

func (s *SearchSettings) AddInArchiveExtension(xs string) {
	s.FindSettings.AddInArchiveExtension(xs)
}

func (s *SearchSettings) InArchiveFilePatterns() *gofind.Patterns {
	return s.FindSettings.InArchiveFilePatterns()
}

func (s *SearchSettings) AddInArchiveFilePattern(p string) {
	s.FindSettings.AddInArchiveFilePattern(p)
}

func (s *SearchSettings) InDirPatterns() *gofind.Patterns {
	return s.FindSettings.InDirPatterns()
}

func (s *SearchSettings) AddInDirPattern(p string) {
	s.FindSettings.AddInDirPattern(p)
}

func (s *SearchSettings) InExtensions() []string {
	return s.FindSettings.InExtensions()
}

func (s *SearchSettings) AddInExtension(xs string) {
	s.FindSettings.AddInExtension(xs)
}

func (s *SearchSettings) InFilePatterns() *gofind.Patterns {
	return s.FindSettings.InFilePatterns()
}

func (s *SearchSettings) AddInFilePattern(p string) {
	s.FindSettings.AddInFilePattern(p)
}

func (s *SearchSettings) InFileTypes() []gofind.FileType {
	return s.FindSettings.InFileTypes()
}

func (s *SearchSettings) AddInFileType(t gofind.FileType) {
	s.FindSettings.AddInFileType(t)
}

func (s *SearchSettings) IncludeArchives() bool {
	return s.FindSettings.IncludeArchives()
}

func (s *SearchSettings) SetIncludeArchives(b bool) {
	s.FindSettings.SetIncludeArchives(b)
}

func (s *SearchSettings) InLinesAfterPatterns() *gofind.Patterns {
	return s.inLinesAfterPatterns
}

func (s *SearchSettings) AddInLinesAfterPattern(p string) {
	s.inLinesAfterPatterns.AddPatternString(p)
}

func (s *SearchSettings) InLinesBeforePatterns() *gofind.Patterns {
	return s.inLinesBeforePatterns
}

func (s *SearchSettings) AddInLinesBeforePattern(p string) {
	s.inLinesBeforePatterns.AddPatternString(p)
}

func (s *SearchSettings) LinesAfter() int {
	return s.linesAfter
}

func (s *SearchSettings) SetLinesAfter(i int) {
	s.linesAfter = i
}

func (s *SearchSettings) LinesAfterToPatterns() *gofind.Patterns {
	return s.linesAfterToPatterns
}

func (s *SearchSettings) AddLinesAfterToPattern(p string) {
	s.linesAfterToPatterns.AddPatternString(p)
}

func (s *SearchSettings) LinesAfterUntilPatterns() *gofind.Patterns {
	return s.linesAfterUntilPatterns
}

func (s *SearchSettings) AddLinesAfterUntilPattern(p string) {
	s.linesAfterUntilPatterns.AddPatternString(p)
}

func (s *SearchSettings) LinesBefore() int {
	return s.linesBefore
}

func (s *SearchSettings) SetLinesBefore(i int) {
	s.linesBefore = i
}

func (s *SearchSettings) ListDirs() bool {
	return s.FindSettings.ListDirs()
}

func (s *SearchSettings) SetListDirs(b bool) {
	s.FindSettings.SetListDirs(b)
}

func (s *SearchSettings) ListFiles() bool {
	return s.FindSettings.ListFiles()
}

func (s *SearchSettings) SetListFiles(b bool) {
	s.FindSettings.SetListFiles(b)
}

func (s *SearchSettings) ListLines() bool {
	return s.listLines
}

func (s *SearchSettings) SetListLines(b bool) {
	s.listLines = b
}

func (s *SearchSettings) MaxDepth() int {
	return s.FindSettings.MaxDepth()
}

func (s *SearchSettings) SetMaxDepth(i int) {
	s.FindSettings.SetMaxDepth(i)
}

func (s *SearchSettings) SetMaxDepthFromString(depthStr string) {
	s.FindSettings.SetMaxDepthFromString(depthStr)
}

func (s *SearchSettings) MaxLastMod() time.Time {
	return s.FindSettings.MaxLastMod()
}

func (s *SearchSettings) SetMaxLastMod(t time.Time) {
	s.FindSettings.SetMaxLastMod(t)
}

func (s *SearchSettings) SetMaxLastModFromString(timeStr string) {
	s.FindSettings.SetMaxLastModFromString(timeStr)
}

func (s *SearchSettings) MaxLineLength() int {
	return s.maxLineLength
}

func (s *SearchSettings) SetMaxLineLength(i int) {
	s.maxLineLength = i
}

func (s *SearchSettings) MaxSize() int64 {
	return s.FindSettings.MaxSize()
}

func (s *SearchSettings) SetMaxSize(i int64) {
	s.FindSettings.SetMaxSize(i)
}

func (s *SearchSettings) SetMaxSizeFromString(sizeStr string) {
	s.FindSettings.SetMaxSizeFromString(sizeStr)
}

func (s *SearchSettings) MinDepth() int {
	return s.FindSettings.MinDepth()
}

func (s *SearchSettings) SetMinDepth(i int) {
	s.FindSettings.SetMinDepth(i)
}

func (s *SearchSettings) SetMinDepthFromString(depthStr string) {
	s.FindSettings.SetMinDepthFromString(depthStr)
}

func (s *SearchSettings) MinLastMod() time.Time {
	return s.FindSettings.MinLastMod()
}

func (s *SearchSettings) SetMinLastMod(t time.Time) {
	s.FindSettings.SetMinLastMod(t)
}

func (s *SearchSettings) SetMinLastModFromString(timeStr string) {
	s.FindSettings.SetMinLastModFromString(timeStr)
}

func (s *SearchSettings) MinSize() int64 {
	return s.FindSettings.MinSize()
}

func (s *SearchSettings) SetMinSize(i int64) {
	s.FindSettings.SetMinSize(i)
}

func (s *SearchSettings) SetMinSizeFromString(sizeStr string) {
	s.FindSettings.SetMinSizeFromString(sizeStr)
}

func (s *SearchSettings) MultiLineSearch() bool {
	return s.multiLineSearch
}

func (s *SearchSettings) SetMultiLineSearch(b bool) {
	s.multiLineSearch = b
}

func (s *SearchSettings) OutArchiveExtensions() []string {
	return s.FindSettings.OutArchiveExtensions()
}

func (s *SearchSettings) AddOutArchiveExtension(xs string) {
	s.FindSettings.AddOutArchiveExtension(xs)
}

func (s *SearchSettings) OutArchiveFilePatterns() *gofind.Patterns {
	return s.FindSettings.OutArchiveFilePatterns()
}

func (s *SearchSettings) AddOutArchiveFilePattern(p string) {
	s.FindSettings.AddOutArchiveFilePattern(p)
}

func (s *SearchSettings) OutDirPatterns() *gofind.Patterns {
	return s.FindSettings.OutDirPatterns()
}

func (s *SearchSettings) AddOutDirPattern(p string) {
	s.FindSettings.AddOutDirPattern(p)
}

func (s *SearchSettings) OutExtensions() []string {
	return s.FindSettings.OutExtensions()
}

func (s *SearchSettings) AddOutExtension(xs string) {
	s.FindSettings.AddOutExtension(xs)
}

func (s *SearchSettings) OutFilePatterns() *gofind.Patterns {
	return s.FindSettings.OutFilePatterns()
}

func (s *SearchSettings) AddOutFilePattern(p string) {
	s.FindSettings.AddOutFilePattern(p)
}

func (s *SearchSettings) OutFileTypes() []gofind.FileType {
	return s.FindSettings.OutFileTypes()
}

func (s *SearchSettings) AddOutFileType(t gofind.FileType) {
	s.FindSettings.AddOutFileType(t)
}

func (s *SearchSettings) OutLinesAfterPatterns() *gofind.Patterns {
	return s.outLinesAfterPatterns
}

func (s *SearchSettings) AddOutLinesAfterPattern(p string) {
	s.outLinesAfterPatterns.AddPatternString(p)
}

func (s *SearchSettings) OutLinesBeforePatterns() *gofind.Patterns {
	return s.outLinesBeforePatterns
}

func (s *SearchSettings) AddOutLinesBeforePattern(p string) {
	s.outLinesBeforePatterns.AddPatternString(p)
}

func (s *SearchSettings) Paths() []string {
	return s.FindSettings.Paths()
}

func (s *SearchSettings) AddPath(p string) {
	s.FindSettings.AddPath(p)
}

func (s *SearchSettings) PrintResults() bool {
	return s.printResults
}

func (s *SearchSettings) SetPrintResults(b bool) {
	s.printResults = b
}

func (s *SearchSettings) PrintUsage() bool {
	return s.FindSettings.PrintUsage()
}

func (s *SearchSettings) SetPrintUsage(b bool) {
	s.FindSettings.SetPrintUsage(b)
}

func (s *SearchSettings) PrintVersion() bool {
	return s.FindSettings.PrintVersion()
}

func (s *SearchSettings) SetPrintVersion(b bool) {
	s.FindSettings.SetPrintVersion(b)
}

func (s *SearchSettings) Recursive() bool {
	return s.FindSettings.Recursive()
}

func (s *SearchSettings) SetRecursive(b bool) {
	s.FindSettings.SetRecursive(b)
}

func (s *SearchSettings) SearchArchives() bool {
	return s.searchArchives
}

func (s *SearchSettings) SetSearchArchives(b bool) {
	s.searchArchives = b
}

func (s *SearchSettings) SearchPatterns() *gofind.Patterns {
	return s.searchPatterns
}

func (s *SearchSettings) AddSearchPattern(p string) {
	s.searchPatterns.AddPatternString(p)
}

func (s *SearchSettings) SortBy() gofind.SortBy {
	return s.FindSettings.SortBy()
}

func (s *SearchSettings) SetSortBy(sortBy gofind.SortBy) {
	s.FindSettings.SetSortBy(sortBy)
}

func (s *SearchSettings) SetSortByFromString(sortByStr string) {
	s.FindSettings.SetSortBy(gofind.SortByForName(sortByStr))
}

func (s *SearchSettings) SortCaseInsensitive() bool {
	return s.FindSettings.SortCaseInsensitive()
}

func (s *SearchSettings) SetSortCaseInsensitive(b bool) {
	s.FindSettings.SetSortCaseInsensitive(b)
}

func (s *SearchSettings) SortDescending() bool {
	return s.FindSettings.SortDescending()
}

func (s *SearchSettings) SetSortDescending(b bool) {
	s.FindSettings.SetSortDescending(b)
}

func (s *SearchSettings) TextFileEncoding() string {
	return s.textFileEncoding
}

func (s *SearchSettings) SetTextFileEncoding(enc string) {
	s.textFileEncoding = enc
}

func (s *SearchSettings) UniqueLines() bool {
	return s.uniqueLines
}

func (s *SearchSettings) SetUniqueLines(b bool) {
	s.uniqueLines = b
}

func (s *SearchSettings) Verbose() bool {
	return s.FindSettings.Verbose()
}

func (s *SearchSettings) SetVerbose(b bool) {
	s.FindSettings.SetVerbose(b)
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
		", MaxDepth: %s" +
		", MaxLastMod: %s" +
		", MaxLineLength: %d" +
		", MaxSize: %d" +
		", MinDepth: %s" +
		", MinLastMod: %s" +
		", MinSize: %d" +
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
		", SortBy: %s" +
		", SortCaseInsensitive: %t" +
		", SortDescending: %t" +
		", TextFileEncoding: \"%s\"" +
		", UniqueLines: %t" +
		", Verbose: %t}"
	return fmt.Sprintf(template,
		s.ArchivesOnly(),
		s.Colorize(),
		s.Debug(),
		s.ExcludeHidden(),
		s.FirstMatch(),
		gofind.StringListToString(s.InArchiveExtensions()),
		gofind.PatternsToString(s.InArchiveFilePatterns()),
		gofind.PatternsToString(s.InDirPatterns()),
		gofind.StringListToString(s.InExtensions()),
		gofind.PatternsToString(s.InFilePatterns()),
		gofind.FileTypeListToString(s.InFileTypes()),
		gofind.PatternsToString(s.InLinesAfterPatterns()),
		gofind.PatternsToString(s.InLinesBeforePatterns()),
		s.LinesAfter(),
		gofind.PatternsToString(s.LinesAfterToPatterns()),
		gofind.PatternsToString(s.LinesAfterUntilPatterns()),
		s.LinesBefore(),
		s.ListDirs(),
		s.ListFiles(),
		s.ListLines(),
		s.MaxDepth(),
		gofind.LastModToString(s.MaxLastMod()),
		s.MaxLineLength(),
		s.MaxSize(),
		s.MinDepth(),
		gofind.LastModToString(s.MinLastMod()),
		s.MinSize(),
		s.MultiLineSearch(),
		gofind.StringListToString(s.OutArchiveExtensions()),
		gofind.PatternsToString(s.OutArchiveFilePatterns()),
		gofind.PatternsToString(s.OutDirPatterns()),
		gofind.StringListToString(s.OutExtensions()),
		gofind.PatternsToString(s.OutFilePatterns()),
		gofind.FileTypeListToString(s.OutFileTypes()),
		gofind.PatternsToString(s.OutLinesAfterPatterns()),
		gofind.PatternsToString(s.OutLinesBeforePatterns()),
		gofind.StringListToString(s.Paths()),
		s.PrintResults(),
		s.FindSettings.PrintUsage(),
		s.FindSettings.PrintVersion(),
		s.FindSettings.Recursive(),
		s.SearchArchives(),
		gofind.PatternsToString(s.SearchPatterns()),
		s.FindSettings.SortBy(),
		s.FindSettings.SortCaseInsensitive(),
		s.FindSettings.SortDescending(),
		s.TextFileEncoding(),
		s.UniqueLines(),
		s.FindSettings.Verbose(),
	)
}
