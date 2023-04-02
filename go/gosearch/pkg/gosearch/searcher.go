// Package gosearch /*
package gosearch

import (
	"archive/tar"
	"archive/zip"
	"bufio"
	"compress/bzip2"
	"compress/gzip"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"

	"golang.org/x/text/encoding"
	"golang.org/x/text/encoding/ianaindex"
	"golang.org/x/text/transform"
)

type Searcher struct {
	Settings           *SearchSettings
	fileTypes          *FileTypes
	searchDirs         []string
	searchItems        *SearchItems
	errors             []error
	addItemChan        chan *SearchItem
	addItemsDoneChan   chan bool
	fileSearchedChan   chan string
	errChan            chan error
	addSearchItemsDone bool
	searchDone         bool
	searchResults      *SearchResults
	resultChan         chan *SearchResult
	textDecoder        *encoding.Decoder
}

func NewSearcher(settings *SearchSettings) *Searcher {
	return &Searcher{
		settings,                   // Settings
		FileTypesFromJson(),        // fileTypes
		[]string{},                 // searchDirs
		NewSearchItems(),           // searchItems
		[]error{},                  // errors
		make(chan *SearchItem),     // addItemChan
		make(chan bool),            // addItemsDoneChan
		make(chan string, 1),       // fileSearchedChan
		make(chan error, 1),        // errChan
		false,                      // addSearchItemsDone
		false,                      // searchDone
		NewSearchResults(settings), // searchResults
		make(chan *SearchResult),   // resultChan
		nil,
	}
}

func (s *Searcher) ClearSearchResults() {
	s.searchResults = nil
}

func (s *Searcher) GetSearchResults() *SearchResults {
	return s.searchResults
}

func (s *Searcher) validateSettings() error {
	if len(s.Settings.Paths) < 1 {
		return fmt.Errorf("Startpath not defined")
	}

	for _, p := range s.Settings.Paths {
		fi, err := os.Stat(p)
		if err != nil {
			if os.IsNotExist(err) {
				return fmt.Errorf("Startpath not found")
			}
			if os.IsPermission(err) {
				return fmt.Errorf("Startpath not readable")
			}
			return err
		}
		if fi.IsDir() && !s.isSearchDir(p) {
			return fmt.Errorf("Startpath does not match search settings")
		} else if fi.Mode().IsRegular() {
			dir, file := filepath.Split(p)
			if !s.isSearchDir(dir) || !s.isSearchFile(file) {
				return fmt.Errorf("Startpath does not match search settings")
			}
		}
	}

	if s.Settings.SearchPatterns.IsEmpty() {
		return fmt.Errorf("No search patterns defined")
	}
	if s.Settings.LinesAfter < 0 {
		return fmt.Errorf("Invalid linesafter")
	}
	if s.Settings.LinesBefore < 0 {
		return fmt.Errorf("Invalid linesbefore")
	}
	if s.Settings.MaxLineLength < 0 {
		return fmt.Errorf("Invalid maxlinelength")
	}
	enc, err := ianaindex.IANA.Encoding(s.Settings.TextFileEncoding)
	if err != nil {
		return fmt.Errorf("Invalid or unsupported text file encoding")
	}
	s.textDecoder = enc.NewDecoder()
	return nil
}

func filterInBySearchPatterns(s string, inPatterns *SearchPatterns,
	outPatterns *SearchPatterns) bool {
	if !inPatterns.IsEmpty() && !inPatterns.MatchesAny(s) {
		return false
	}
	if !outPatterns.IsEmpty() && outPatterns.MatchesAny(s) {
		return false
	}
	return true
}

func (s *Searcher) isSearchDir(d string) bool {
	if s.Settings.ExcludeHidden && isHidden(d) {
		return false
	}
	return (s.Settings.InDirPatterns.IsEmpty() || s.Settings.InDirPatterns.MatchesAny(d)) &&
		(s.Settings.OutDirPatterns.IsEmpty() || !s.Settings.OutDirPatterns.MatchesAny(d))
}

func (s *Searcher) isArchiveSearchFile(filename string) bool {
	if s.fileTypes.IsArchiveFile(filename) {
		if isHidden(filename) && s.Settings.ExcludeHidden {
			return false
		}
		ext := getExtension(filename)
		if len(s.Settings.InArchiveExtensions) > 0 && !contains(s.Settings.InArchiveExtensions, ext) {
			return false
		}
		if len(s.Settings.OutArchiveExtensions) > 0 && contains(s.Settings.OutArchiveExtensions, ext) {
			return false
		}
		return filterInBySearchPatterns(filename, s.Settings.InArchiveFilePatterns,
			s.Settings.OutArchiveFilePatterns)
	}
	return false
}

func (s *Searcher) isArchiveSearchItem(si *SearchItem) bool {
	if si.fileType == FiletypeArchive {
		if s.Settings.ExcludeHidden && (isHidden(si.Path) || isHidden(si.Name)) {
			return false
		}
		if len(s.Settings.InArchiveExtensions) > 0 || len(s.Settings.OutArchiveExtensions) > 0 {
			ext := getExtension(si.Name)
			if len(s.Settings.InArchiveExtensions) > 0 && !contains(s.Settings.InArchiveExtensions, ext) {
				return false
			}
			if len(s.Settings.OutArchiveExtensions) > 0 && contains(s.Settings.OutArchiveExtensions, ext) {
				return false
			}
		}
		return (s.Settings.InArchiveFilePatterns.IsEmpty() || s.Settings.InArchiveFilePatterns.MatchesAny(si.Name)) &&
			(s.Settings.OutArchiveFilePatterns.IsEmpty() || !s.Settings.OutArchiveFilePatterns.MatchesAny(si.Name))
	}
	return false
}

func (s *Searcher) isSearchFile(filename string) bool {
	if isHidden(filename) && s.Settings.ExcludeHidden {
		return false
	}
	if len(s.Settings.InExtensions) > 0 || len(s.Settings.OutExtensions) > 0 {
		ext := getExtension(filename)
		if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
			return false
		}
		if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
			return false
		}
	}
	fileType := s.fileTypes.getFileType(filename)
	if len(s.Settings.InFileTypes) > 0 && !containsFileType(s.Settings.InFileTypes, fileType) {
		return false
	}
	if len(s.Settings.OutFileTypes) > 0 && containsFileType(s.Settings.OutFileTypes, fileType) {
		return false
	}
	return filterInBySearchPatterns(filename, s.Settings.InFilePatterns,
		s.Settings.OutFilePatterns)
}

func (s *Searcher) isSearchItem(si *SearchItem) bool {
	if s.Settings.ExcludeHidden && (isHidden(si.Path) || isHidden(si.Name)) {
		return false
	}
	if len(s.Settings.InExtensions) > 0 || len(s.Settings.OutExtensions) > 0 {
		ext := getExtension(si.Name)
		if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
			return false
		}
		if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
			return false
		}
	}
	if len(s.Settings.InFileTypes) > 0 && !containsFileType(s.Settings.InFileTypes, si.fileType) {
		return false
	}
	if len(s.Settings.OutFileTypes) > 0 && containsFileType(s.Settings.OutFileTypes, si.fileType) {
		return false
	}
	return filterInBySearchPatterns(si.Name, s.Settings.InFilePatterns,
		s.Settings.OutFilePatterns)
}

func (s *Searcher) filterFile(f string) bool {
	if s.fileTypes.IsArchiveFile(f) {
		return s.Settings.SearchArchives && s.isArchiveSearchFile(f)
	}
	return !s.Settings.ArchivesOnly && s.isSearchFile(f)
}

func (s *Searcher) filterSearchItem(si *SearchItem) bool {
	if si.fileType == FiletypeArchive {
		return s.Settings.SearchArchives && s.isArchiveSearchItem(si)
	}
	return !s.Settings.ArchivesOnly && s.isSearchItem(si)
}

func (s *Searcher) fileToSearchItem(f string) *SearchItem {
	dir, file := filepath.Split(f)
	if dir == "" {
		dir = "."
	} else {
		dir = normalizePath(dir)
	}
	t := s.fileTypes.getFileType(file)
	return NewSearchItem(dir, file, t)
}

func (s *Searcher) checkAddSearchFile(f string) {
	searchItem := s.fileToSearchItem(f)
	if s.filterSearchItem(searchItem) {
		s.addItemChan <- searchItem
	}
}

// this method passed to the filepath.Walk method, it must have this signature
func (s *Searcher) checkAddSearchWalkFile(filePath string, fi os.FileInfo, err error) error {
	if err != nil {
		fmt.Printf("prevent panic by handling failure accessing a path %q: %v\n", filePath, err)
		return err
	}
	if fi.IsDir() && !s.isSearchDir(fi.Name()) {
		return filepath.SkipDir
	} else if fi.Mode().IsRegular() {
		s.checkAddSearchFile(filePath)
	}
	return nil
}

//func toFindPatterns(searchPatterns *SearchPatterns) *gofind.FindPatterns {
//	findPatterns := gofind.NewFindPatterns()
//	for _, p := range searchPatterns.patterns {
//		findPatterns.AddPattern(p)
//	}
//
//	return findPatterns
//}

//func toFindSettings(ss *SearchSettings) *gofind.FindSettings {
//	return &gofind.FindSettings{
//		ss.ArchivesOnly,
//		ss.Debug,
//		ss.ExcludeHidden,
//		ss.InArchiveExtensions,
//		toFindPatterns(ss.InArchiveFilePatterns),
//		toFindPatterns(ss.InDirPatterns),
//		ss.InExtensions,
//		toFindPatterns(ss.InFilePatterns),
//		ss.InFileTypes,
//		ss.SearchArchives,
//		ss.ListDirs,
//		ss.ListFiles,
//		time.Time{}, // MaxLastMod
//		0,           // MaxSize
//		time.Time{}, // MinLastMod
//		0,           // MinSize
//		ss.OutArchiveExtensions,
//		toFindPatterns(ss.OutArchiveFilePatterns),
//		toFindPatterns(ss.OutDirPatterns),
//		ss.OutExtensions,
//		toFindPatterns(ss.OutFilePatterns),
//		ss.OutFileTypes,
//		ss.Paths,
//		ss.PrintUsage,
//		ss.PrintVersion,
//		ss.Recursive,
//		gofind.SortByFilepath, // SortBy
//		false,
//		false,
//		ss.Verbose,
//	}
//}

func (s *Searcher) setSearchFiles() error {
	if s.Settings.Verbose {
		log("\nBuilding file search list")
	}

	//// TODO: init finder with Settings that pass as FindSettings
	//finder := gofind.NewFinder(toFindSettings(s.Settings))

	for _, p := range s.Settings.Paths {
		normPath := normalizePath(p)
		fi, err := os.Stat(normPath)
		if err != nil {
			return err
		}
		if fi.IsDir() {
			if s.Settings.Recursive {
				err := filepath.Walk(normPath, s.checkAddSearchWalkFile)
				if err != nil {
					return err
				}
			} else {
				entries, err := os.ReadDir(normPath)
				if err != nil {
					return err
				}

				for _, entry := range entries {
					if !entry.IsDir() {
						s.checkAddSearchFile(filepath.Join(p, entry.Name()))
					}
				}
			}
		} else if fi.Mode().IsRegular() {
			s.checkAddSearchFile(normPath)
		}
	}

	s.addItemsDoneChan <- true
	return nil
}

func (s *Searcher) addSearchResult(r *SearchResult) {
	s.searchResults.AddSearchResult(r)
}

func linesMatch(lines []string, inPatterns *SearchPatterns,
	outPatterns *SearchPatterns) bool {
	inLinesMatch := inPatterns.IsEmpty() || inPatterns.AnyMatchesAny(lines)
	outLinesMatch := !outPatterns.IsEmpty() && outPatterns.AnyMatchesAny(lines)
	return inLinesMatch && !outLinesMatch
}

func (s *Searcher) linesAfterMatch(linesAfter []string) bool {
	return linesMatch(linesAfter, s.Settings.InLinesAfterPatterns,
		s.Settings.OutLinesAfterPatterns)
}

func (s *Searcher) linesBeforeMatch(linesBefore []string) bool {
	return linesMatch(linesBefore, s.Settings.InLinesBeforePatterns,
		s.Settings.OutLinesBeforePatterns)
}

func hasNewLine(bytes []byte) bool {
	for _, b := range bytes {
		if b == '\n' {
			return true
		}
	}
	return false
}

func getNewLineCount(bytes []byte) int {
	count := 0
	for _, b := range bytes {
		if b == '\n' {
			count++
		}
	}
	return count
}

func newlineIndices(bytes []byte) []int {
	var newlineidxs []int
	for i, b := range bytes {
		if b == '\n' {
			newlineidxs = append(newlineidxs, i)
		}
	}
	return newlineidxs
}

func lineStartEndIndicesForIndex(idx int, bytes []byte) (int, int) {
	startidx, endidx := idx, idx
	for startidx > 0 && bytes[startidx] != '\n' {
		startidx--
	}
	if bytes[startidx] == '\n' {
		startidx++
	}
	for endidx < len(bytes) && bytes[endidx] != '\n' {
		endidx++
	}
	if endidx < len(bytes) && bytes[endidx] == '\n' {
		endidx++
	}
	if startidx == endidx && startidx > 0 {
		startidx--
	}
	return startidx, endidx
}

func splitIntoLines(bytes []byte) []string {
	newlineidxs := newlineIndices(bytes)
	emptyStr := ""
	var lines []string
	startidx, endidx := 0, 0
	for _, n := range newlineidxs {
		endidx = n
		if startidx == endidx {
			lines = append(lines, emptyStr)
		} else if startidx < endidx {
			nextline := string(bytes[startidx:endidx])
			lines = append(lines, nextline)
		}
		startidx = endidx + 1
	}
	endidx = len(bytes) - 1
	if bytes[endidx] == '\n' {
		lines = append(lines, emptyStr)
	}
	return lines
}

func linesBeforeIndex(bytes []byte, idx int, lineCount int) []string {
	var lines []string
	if idx < 1 {
		return lines
	}
	newlines := 0
	beforeidx := idx
	for beforeidx > 0 && newlines < lineCount {
		if bytes[beforeidx] == '\n' {
			newlines++
		}
		beforeidx--
	}
	beforestartlineidx, _ := lineStartEndIndicesForIndex(beforeidx, bytes)
	lines = splitIntoLines(bytes[beforestartlineidx : idx-1])
	return lines
}

func linesAfterIndex(bytes []byte, idx int, lineCount int) []string {
	var lines []string
	newlines := 0
	afteridx := idx
	for afteridx < len(bytes)-1 && newlines < lineCount {
		if bytes[afteridx] == '\n' {
			newlines++
		}
		afteridx++
	}
	_, afterendlineidx := lineStartEndIndicesForIndex(afteridx-1, bytes)
	lines = splitIntoLines(bytes[idx:afterendlineidx])
	return lines[:lineCount]
}

func (s *Searcher) searchTextFileReaderContents(r io.Reader, si *SearchItem) {
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		s.errChan <- err
		return
	}
	results := s.searchTextBytes(bytes)
	for _, sr := range results {
		sr.File = si
		s.resultChan <- sr
	}
}

// public method to search a multiline string
func (s *Searcher) SearchMultiLineString(str string) []*SearchResult {
	return s.searchTextBytes([]byte(str))
}

func (s *Searcher) searchTextBytes(bytes []byte) []*SearchResult {
	var results []*SearchResult
	var linesBefore []string
	var linesAfter []string
	findLimit := -1
	if s.Settings.FirstMatch {
		findLimit = 1
	}
	spi := s.Settings.SearchPatterns.Iterator()
	for spi.Next() {
		p := spi.Value()
		if allIndices := p.FindAllIndex(bytes, findLimit); allIndices != nil {
			for _, idx := range allIndices {
				// get the start and end indices of the current line
				startidx, endidx := lineStartEndIndicesForIndex(idx[0], bytes)
				// grab the contents in that range as the line
				line := bytes[startidx:endidx]
				linenum, beforeLineCount, afterLineCount := 1, 0, 0
				if hasNewLine(bytes[0:startidx]) {
					beforeLineCount = getNewLineCount(bytes[0:startidx])
					linenum = beforeLineCount + 1
				}
				if hasNewLine(bytes[endidx:]) {
					afterLineCount = getNewLineCount(bytes[endidx:])
				}
				if s.Settings.LinesBefore > 0 && beforeLineCount > 0 {
					linesBefore = linesBeforeIndex(bytes, startidx, s.Settings.LinesBefore)
				}
				if s.Settings.LinesAfter > 0 && afterLineCount > 0 {
					linesAfter = linesAfterIndex(bytes, endidx, s.Settings.LinesAfter)
				}

				if len(linesBefore) > 0 && !s.linesBeforeMatch(linesBefore) {
					continue
				} else if len(linesAfter) > 0 && !s.linesAfterMatch(linesAfter) {
					continue
				}

				lineStr := strings.TrimRight(string(line), "\r\n")
				sr := &SearchResult{
					p,
					nil,
					linenum,
					idx[0] - startidx + 1,
					idx[1] - startidx + 1,
					lineStr,
					linesBefore,
					linesAfter,
				}
				results = append(results, sr)

				// reset linesBefore and LinesAfter
				linesBefore, linesAfter = []string{}, []string{}
			}
		}
	}
	return results
}

func (s *Searcher) searchTextFileReaderLines(r io.Reader, si *SearchItem) {
	results := s.SearchTextReaderLines(r)
	for _, sr := range results {
		sr.File = si
		s.resultChan <- sr
	}
}

func (s *Searcher) SearchTextReaderLines(r io.Reader) []*SearchResult {
	var results []*SearchResult
	scanner := bufio.NewScanner(r)
	linenum := 0
	var linesBefore []string
	var linesAfter []string
	linesAfterIdx := 0
	patternMatches := map[*regexp.Regexp]int{}
ReadLines:
	for {
		linenum++
		var line string
		if len(linesAfter) > 0 {
			line, linesAfter = linesAfter[0], linesAfter[1:]
		} else if scanner.Scan() {
			text := scanner.Text()
			line = text
		} else {
			break ReadLines
		}
		for len(linesAfter) < s.Settings.LinesAfter && scanner.Scan() {
			lineAfter := scanner.Text()
			linesAfter = append(linesAfter, lineAfter)
		}
		spi := s.Settings.SearchPatterns.Iterator()
		for spi.Next() {
			p := spi.Value()
			if matchIndices := p.FindAllStringIndex(line, -1); matchIndices != nil {
				if len(linesBefore) > 0 && !s.linesBeforeMatch(linesBefore) {
					continue
				} else if len(linesAfter) > 0 && !s.linesAfterMatch(linesAfter) {
					continue
				}
				linesAfterToMatch := false
				linesAfterUntilMatch := false
				if !s.Settings.LinesAfterToPatterns.IsEmpty() ||
					!s.Settings.LinesAfterUntilPatterns.IsEmpty() {

					if !s.Settings.LinesAfterToPatterns.IsEmpty() &&
						s.Settings.LinesAfterToPatterns.AnyMatchesAny(linesAfter) {
						linesAfterToMatch = true
					}
					if !s.Settings.LinesAfterUntilPatterns.IsEmpty() &&
						s.Settings.LinesAfterUntilPatterns.AnyMatchesAny(linesAfter) {
						linesAfterUntilMatch = true
					}

					for !linesAfterToMatch && !linesAfterUntilMatch {
						if len(linesAfter) < linesAfterIdx+1 {
							if !scanner.Scan() {
								break
							}
							lineAfter := scanner.Text()
							linesAfter = append(linesAfter, lineAfter)
						}
						nextLine := linesAfter[linesAfterIdx]
						if !s.Settings.LinesAfterToPatterns.IsEmpty() &&
							s.Settings.LinesAfterToPatterns.MatchesAny(nextLine) {
							linesAfterToMatch = true
						}
						if !s.Settings.LinesAfterUntilPatterns.IsEmpty() &&
							s.Settings.LinesAfterUntilPatterns.MatchesAny(nextLine) {
							linesAfterUntilMatch = true
						}
						linesAfterIdx++
					}
				}
				var srLinesAfter []string
				if linesAfterIdx > 0 {
					lastIdx := linesAfterIdx + 1
					if lastIdx > len(linesAfter) {
						lastIdx = len(linesAfter)
					}
					if !s.Settings.LinesAfterToPatterns.IsEmpty() {
						srLinesAfter = linesAfter[:lastIdx]
					} else if !s.Settings.LinesAfterUntilPatterns.IsEmpty() {
						srLinesAfter = linesAfter[:lastIdx-1]
					}
					linesAfterIdx = 0
				} else {
					srLinesAfter = linesAfter
				}
				// iterate through matchIndices
				for _, m := range matchIndices {
					// check for FirstMatch setting and stop if file+pattern match exists
					_, patternMatched := patternMatches[p]
					if s.Settings.FirstMatch && patternMatched {
						continue
					} else {
						sr := &SearchResult{
							p,
							nil,
							linenum,
							m[0] + 1,
							m[1] + 1,
							line,
							linesBefore,
							srLinesAfter,
						}
						results = append(results, sr)
						patternMatches[p] = 1
					}
				}
			}
		}
		if s.Settings.LinesBefore > 0 {
			if len(linesBefore) == s.Settings.LinesBefore {
				linesBefore = linesBefore[1:]
			}
			if len(linesBefore) < s.Settings.LinesBefore {
				linesBefore = append(linesBefore, line)
			}
		}
	}
	if err := scanner.Err(); err != nil {
		s.errChan <- err
	}
	return results
}

func (s *Searcher) searchTextFileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching text file %s", si.String()))
	}
	if s.Settings.MultiLineSearch {
		s.searchTextFileReaderContents(r, si)
	} else {
		s.searchTextFileReaderLines(r, si)
	}
}

func (s *Searcher) searchBinaryFileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching binary file %s", si.String()))
	}
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		s.errChan <- err
		return
	}
	findLimit := -1
	if s.Settings.FirstMatch {
		findLimit = 1
	}
	spi := s.Settings.SearchPatterns.Iterator()
	for spi.Next() {
		p := spi.Value()
		if matchIndices := p.FindAllIndex(bytes, findLimit); matchIndices != nil {
			for _, m := range matchIndices {
				emptyStr := ""
				sr := &SearchResult{
					p,
					si,
					0,
					m[0] + 1,
					m[1] + 1,
					emptyStr,
					[]string{},
					[]string{},
				}
				s.resultChan <- sr
			}
		}
	}
}

func notR(c rune) bool {
	return c != 'r'
}

func (s *Searcher) searchTarFileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		tarName := strings.TrimRightFunc(si.String(), notR)
		log(fmt.Sprintf("Searching tar file %s", tarName))
	}
	tr := tar.NewReader(r)
	for {
		hdr, err := tr.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			if err == io.ErrUnexpectedEOF {
				if s.Settings.Debug {
					log(fmt.Sprintf("Encountered unexpected EOF in tar file %s",
						si.String()))
				}
				break
			}
			if s.Settings.Debug {
				log(fmt.Sprintf("Error encountered in searchTarFileReader: %s",
					err))
			}
			s.errChan <- err
		}
		dir, file := filepath.Split(hdr.Name)
		if !strings.HasSuffix(hdr.Name, "/") {
			if s.isSearchFile(file) {
				t := s.fileTypes.getFileType(file)
				newSearchItem := NewSearchItem(dir, file, t)
				for _, c := range si.Containers {
					newSearchItem.AddContainer(c)
				}
				newSearchItem.AddContainer(filepath.Join(si.Path, si.Name))
				s.searchFileReader(tr, newSearchItem)
			} else if s.isArchiveSearchFile(file) {
				t := s.fileTypes.getFileType(file)
				newSearchItem := NewSearchItem(dir, file, t)
				for _, c := range si.Containers {
					newSearchItem.AddContainer(c)
				}
				newSearchItem.AddContainer(filepath.Join(si.Path, si.Name))
				s.searchArchiveFileReader(tr, newSearchItem)
			}
		}
	}
}

func (s *Searcher) searchGzipFileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching gzip file %s", si.String()))
	}
	gr, err := gzip.NewReader(r)
	if err != nil {
		if s.Settings.Debug {
			log(fmt.Sprintf("Error encountered in searchGzipFileReader: %s",
				err))
		}
		s.errChan <- err
		return
	}
	defer func() {
		gr.Close()
	}()
	if strings.HasSuffix(si.Name, "tar.gz") || strings.HasSuffix(si.Name, "tgz") {
		s.searchTarFileReader(gr, si)
	} else {
		name := gr.Name
		if s.isSearchFile(name) {
			emptyStr := ""
			t := s.fileTypes.getFileType(name)
			newSearchItem := NewSearchItem(emptyStr, name, t)
			for _, c := range si.Containers {
				newSearchItem.AddContainer(c)
			}
			newSearchItem.AddContainer(filepath.Join(si.Path, si.Name))
			s.searchFileReader(gr, newSearchItem)
		}
	}
}

func (s *Searcher) searchBzip2FileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching bzip2 file %s", si.String()))
	}
	br := bzip2.NewReader(r)
	if strings.HasSuffix(si.Name, "tar.bz2") {
		s.searchTarFileReader(br, si)
	} else {
		containedFileName := strings.TrimSuffix(si.Name, ".bz2")
		if s.isSearchFile(containedFileName) {
			emptyStr := ""
			t := s.fileTypes.getFileType(containedFileName)
			newSearchItem := NewSearchItem(emptyStr, containedFileName, t)
			for _, c := range si.Containers {
				newSearchItem.AddContainer(c)
			}
			newSearchItem.AddContainer(filepath.Join(si.Path, si.Name))
			s.searchFileReader(br, newSearchItem)
		}
	}
}

func (s *Searcher) searchZipFileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching zip file %s", si.String()))
	}
	// zip.OpenReader returns a *zip.ReaderCloser struct type that extends Reader
	zr, err := zip.OpenReader(filepath.Join(si.Path, si.Name))
	if err != nil {
		s.errChan <- err
		return
	}
	defer func(zr *zip.ReadCloser) {
		err := zr.Close()
		if err != nil {

		}
	}(zr)
	// f is a zip.File struct type
	for _, f := range zr.File {
		dir, file := filepath.Split(f.Name)
		if f.FileHeader.Flags != 0 && f.FileHeader.Flags != 2 {
			log(fmt.Sprintf("%s is an UNKNOWN file type", file))
		}
		// f.FileHeader.Flags == 2 seems to mean it's a file (not a dir, etc.)
		if f.FileHeader.Flags == 2 && s.isSearchFile(file) {
			cr, err := f.Open()
			if err != nil {
				s.errChan <- err
				return
			}
			t := s.fileTypes.getFileType(file)
			newSearchItem := NewSearchItem(dir, file, t)
			for _, c := range si.Containers {
				newSearchItem.AddContainer(c)
			}
			newSearchItem.AddContainer(filepath.Join(si.Path, si.Name))
			s.searchFileReader(cr, newSearchItem)
			cr.Close()
		}
	}
}

func (s *Searcher) searchArchiveFileReader(r io.Reader, si *SearchItem) {
	if !s.isArchiveSearchFile(si.Name) {
		return
	}
	ext := getExtension(si.Name)
	switch ext {
	case "zip", "jar", "war", "ear":
		s.searchZipFileReader(r, si)
	case "gz", "tgz":
		s.searchGzipFileReader(r, si)
	case "bz2":
		s.searchBzip2FileReader(r, si)
	default:
		log(fmt.Sprintf("Searching not currently supported for %s files", ext))
	}
}

func (s *Searcher) searchFileReader(r io.Reader, si *SearchItem) {
	switch si.fileType {
	case FiletypeCode, FiletypeXml, FiletypeText:
		s.searchTextFileReader(transform.NewReader(r, s.textDecoder), si)
	case FiletypeBinary:
		s.searchBinaryFileReader(r, si)
	case FiletypeArchive:
		if s.Settings.SearchArchives {
			s.searchArchiveFileReader(r, si)
		} else {
			if s.Settings.Verbose {
				log(fmt.Sprintf("Skipping archive file: %s", si.String()))
			}
		}
	default:
		log(fmt.Sprintf("Skipping unknown file type: %s", si.String()))
	}
}

func (s *Searcher) searchSearchItem(si *SearchItem) {
	if !s.fileTypes.IsSearchableItem(si) {
		if contains(s.Settings.InExtensions, getExtension(si.Name)) {
			if s.Settings.Debug {
				log(fmt.Sprintf("File made searchable by passing in-ext: %s",
					si.String()))
			}
		} else {
			if s.Settings.Verbose {
				log(fmt.Sprintf("Skipping unsearchable file: %s", si.String()))
			}
			return
		}
	}
	// make sure it doesn't have any containers (not sure how this could happen)
	if len(si.Containers) > 0 {
		log(fmt.Sprintf("Has containers: %s", si.String()))
	} else {
		// create an io.Reader
		fullName := filepath.Join(si.Path, si.Name)
		r, err := os.Open(fullName)
		if err != nil {
			s.errChan <- err
			return
		}
		//defer func(r *os.File) {
		//	err := r.Close()
		//	if err != nil {
		//
		//	}
		//}(r)
		defer r.Close()
		s.searchFileReader(r, si)
		s.fileSearchedChan <- fullName
	}
}

// initiates goroutines to search each file in the batch, waiting for all
// to finish before returning
func (s *Searcher) doBatchFileSearch(searchItems []*SearchItem) {
	wg := &sync.WaitGroup{}
	wg.Add(len(searchItems)) // set the WaitGroup counter to searchItem length
	for _, si := range searchItems {
		go func(wg *sync.WaitGroup, si *SearchItem) {
			s.searchSearchItem(si)
			wg.Done() // decrement the counter
		}(wg, si)
	}
	wg.Wait()
}

func (s *Searcher) doFileSearch() error {
	const batchSize = 240 // max files to search at one time

	// start the activateSearchChannels goroutine
	go s.activateSearchChannels()

	sii := s.searchItems.Iterator()
	var searchItems []*SearchItem
	for sii.HasNext() && len(s.errors) == 0 {
		searchItems = sii.Take(batchSize)
		s.doBatchFileSearch(searchItems)
	}

	if len(s.errors) > 0 {
		return s.errors[0]
	}
	return nil
}

//func (s *Searcher) doFileSearchOld() error {
//	const batchSize = 240 // max files to search at one time
//	var searchItems []*SearchItem
//	sii := s.searchItems.Iterator()
//	for sii.Next() {
//		searchItems = append(searchItems, sii.Value())
//	}
//	// start the activateSearchChannels goroutine
//	go s.activateSearchChannels()
//
//	// batch search (to avoid too many files open at once)
//	for len(searchItems) > batchSize && len(s.errors) == 0 {
//		s.doBatchFileSearch(searchItems[:batchSize])
//		searchItems = searchItems[batchSize:]
//	}
//	if len(searchItems) > 0 && len(s.errors) == 0 {
//		s.doBatchFileSearch(searchItems)
//	}
//	if len(s.errors) > 0 {
//		return s.errors[0]
//	}
//	return nil
//}

func (s *Searcher) activateSearchChannels() {
	// get the results from the results channel
	searchedFiles := map[string]bool{}
	for !s.searchDone {
		select {
		case r := <-s.resultChan:
			s.addSearchResult(r)
		case f := <-s.fileSearchedChan:
			searchedFiles[f] = true
			if len(searchedFiles) == s.searchItems.Count() {
				s.searchDone = true
			}
		case e := <-s.errChan:
			s.errors = append(s.errors, e)
			s.searchDone = true
		}
	}
}

// the public-facing method takes a string and converts to a SearchItem type
func (s *Searcher) SearchFile(fp string) {
	dir, file := filepath.Split(fp)
	t := s.fileTypes.getFileType(file)
	searchItem := NewSearchItem(dir, file, t)
	s.searchSearchItem(searchItem)
}

// get the search items (files) from the file channel
func (s *Searcher) activateSearchItemChannels() {
	for !s.addSearchItemsDone {
		select {
		case b := <-s.addItemsDoneChan:
			s.addSearchItemsDone = b
		case i := <-s.addItemChan:
			s.searchItems.AddItem(i)
		case e := <-s.errChan:
			s.errors = append(s.errors, e)
			s.addSearchItemsDone = true
		}
	}
}

func (s *Searcher) printToBeSearched() {
	var dirs []string
	var files []string

	sfi := s.searchItems.Iterator()
	for sfi.Next() {
		dirs = append(dirs, sfi.Value().Path)
		files = append(files, sfi.Value().String())
	}

	dirMap := makeMap(dirs)
	dirs = getSortedKeys(dirMap)

	log(fmt.Sprintf("\nDirectories to be searched (%d):", len(dirs)))
	for _, d := range dirs {
		log(d)
	}

	fileMap := makeMap(files)
	files = getSortedKeys(fileMap)

	log(fmt.Sprintf("\nFiles to be searched (%d):", len(files)))
	for _, f := range files {
		log(f)
	}
}

func (s *Searcher) Search() error {
	if err := s.validateSettings(); err != nil {
		return err
	}

	// get search file list
	// first start the activateSearchItemChannels goroutine
	go s.activateSearchItemChannels()

	// now fill the searchItem channels
	if err := s.setSearchFiles(); err != nil {
		return err
	}

	if s.Settings.Verbose {
		s.printToBeSearched()
	}

	// search the files
	if s.Settings.Verbose {
		log("\nStarting file search...\n")
	}
	if err := s.doFileSearch(); err != nil {
		return err
	}
	if s.Settings.Verbose {
		log("\nFile search complete.\n")
	}

	return nil
}

func (s *Searcher) PrintSearchResults() {
	s.searchResults.PrintSearchResults()
}

func (s *Searcher) PrintMatchingDirs() {
	s.searchResults.PrintMatchingDirs()
}

func (s *Searcher) PrintMatchingFiles() {
	s.searchResults.PrintMatchingFiles()
}

func (s *Searcher) PrintMatchingLines() {
	s.searchResults.PrintMatchingLines()
}

func (s *Searcher) PrintDirCounts() {
	s.searchResults.PrintDirCounts()
}

func (s *Searcher) PrintFileCounts() {
	s.searchResults.PrintFileCounts()
}

func (s *Searcher) PrintLineCounts() {
	s.searchResults.PrintLineCounts()
}

func (s *Searcher) PrintUniqueLineCounts() {
	s.searchResults.PrintUniqueLineCounts()
}
