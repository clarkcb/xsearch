/*
Package xsearch provides functionality to search specific files in specific
directories for content that matches any number of regular expressions.

The Searcher class is the main class that provides the file searching
functionality. It takes a SearchSettings instance argument on instantiation
that defines the various search options (what files extension, what directory
and/or file name patterns, what content search patterns, etc.).

The two main methods of Searcher are:

* Search - this performs the search based on the SearchSettings, starting in
           StartPath. It has three main phases:

    a) Find matching directories - get the list of directories to search
    b) Find matching files - get the list of files to search under the directories
    c) Search matching files - search the matching files

* SearchFile - this performs a search of a single file. Its use is less common
               but provided for cases where this is needed.
*/
package xsearch

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
)

type Searcher struct {
	Settings         *SearchSettings
	fileTypes        *FileTypes
	searchDirs       []*string
	searchItems      *SearchItems
	errors           []error
	addItemChan      chan *SearchItem
	addItemsDoneChan chan bool
	doneChan         chan *string
	errChan          chan error
	searchResults    *SearchResults
	resultChan       chan *SearchResult
}

func NewSearcher(settings *SearchSettings) *Searcher {
	return &Searcher{
		settings,                 // Settings
		FileTypesFromJson(),      // fileTypes
		[]*string{},              // searchDirs
		NewSearchItems(),         // searchItems
		[]error{},                // errors
		make(chan *SearchItem),   // addItemChan
		make(chan bool),          // addItemsDoneChan
		make(chan *string, 10),   // doneChan
		make(chan error, 1),      // errChan
		NewSearchResults(),       // searchResults
		make(chan *SearchResult), // resultChan
	}
}

func (s *Searcher) ClearSearchResults() {
	s.searchResults = nil
}

func (s *Searcher) GetSearchResults() *SearchResults {
	return s.searchResults
}

func (s *Searcher) validateSettings() error {
	if s.Settings.StartPath == "" {
		return fmt.Errorf("Startpath not defined")
	}
	fi, err := os.Stat(s.Settings.StartPath)
	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf("Startpath not found")
		}
		if os.IsPermission(err) {
			return fmt.Errorf("Startpath not readable")
		}
		return err
	}
	if fi.IsDir() && !s.isSearchDir(&s.Settings.StartPath) {
		return fmt.Errorf("Startpath does not match search settings")
	} else if fi.Mode().IsRegular() {
		dir, file := filepath.Split(s.Settings.StartPath)
		if !s.isSearchDir(&dir) || !s.isSearchFile(&file) {
			return fmt.Errorf("Startpath does not match search settings")
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
	if strings.ToLower(s.Settings.TextFileEncoding) != "utf-8" {
		return fmt.Errorf("Invalid or unsupported text file encoding")
	}
	return nil
}

func filterInBySearchPatterns(s *string, inPatterns *SearchPatterns,
	outPatterns *SearchPatterns) bool {
	if !inPatterns.IsEmpty() && !inPatterns.MatchesAny(s) {
		return false
	}
	if !outPatterns.IsEmpty() && outPatterns.MatchesAny(s) {
		return false
	}
	return true
}

func (s *Searcher) isSearchDir(d *string) bool {
	if isHidden(*d) && s.Settings.ExcludeHidden {
		return false
	}
	return filterInBySearchPatterns(d, s.Settings.InDirPatterns,
		s.Settings.OutDirPatterns)
}

func (s *Searcher) isArchiveSearchFile(filename *string) bool {
	if s.fileTypes.IsArchiveFile(*filename) {
		if isHidden(*filename) && s.Settings.ExcludeHidden {
			return false
		}
		ext := getExtension(*filename)
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
	if si.fileType == FILETYPE_ARCHIVE {
		if (isHidden(*si.Path) || isHidden(*si.Name)) && s.Settings.ExcludeHidden {
			return false
		}
		ext := getExtension(*si.Name)
		if len(s.Settings.InArchiveExtensions) > 0 && !contains(s.Settings.InArchiveExtensions, ext) {
			return false
		}
		if len(s.Settings.OutArchiveExtensions) > 0 && contains(s.Settings.OutArchiveExtensions, ext) {
			return false
		}
		return filterInBySearchPatterns(si.Name, s.Settings.InArchiveFilePatterns,
			s.Settings.OutArchiveFilePatterns)
	}
	return false
}

func (s *Searcher) isSearchFile(filename *string) bool {
	if isHidden(*filename) && s.Settings.ExcludeHidden {
		return false
	}
	ext := getExtension(*filename)
	if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
		return false
	}
	if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
		return false
	}
	fileType := s.fileTypes.getFileType(*filename)
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
	if (isHidden(*si.Path) || isHidden(*si.Name)) && s.Settings.ExcludeHidden {
		return false
	}
	ext := getExtension(*si.Name)
	if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
		return false
	}
	if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
		return false
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

func (s *Searcher) filterFile(f *string) bool {
	if s.fileTypes.IsArchiveFile(*f) {
		return s.Settings.SearchArchives && s.isArchiveSearchFile(f)
	}
	return !s.Settings.ArchivesOnly && s.isSearchFile(f)
}

func (s *Searcher) filterSearchItem(si *SearchItem) bool {
	if si.fileType == FILETYPE_ARCHIVE {
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
	return NewSearchItem(&dir, &file, t)
}

func (s *Searcher) checkAddSearchFile(f string) {
	searchItem := s.fileToSearchItem(f)
	if s.filterSearchItem(searchItem) {
		s.addItemChan <- searchItem
	}
}

// this method passed to the filepath.Walk method, it must have this signature
func (s *Searcher) checkAddSearchWalkFile(f string, fi os.FileInfo, err error) error {
	if fi.Mode().IsRegular() {
		s.checkAddSearchFile(f)
	}
	return nil
}

func (s *Searcher) setSearchFiles() error {
	if s.Settings.Verbose {
		log("\nBuilding file search list")
	}

	startPath := normalizePath(s.Settings.StartPath)
	fi, err := os.Stat(startPath)
	if err != nil {
		return err
	}
	if fi.IsDir() {
		if s.Settings.Recursive {
			err := filepath.Walk(startPath, s.checkAddSearchWalkFile)
			if err != nil {
				return err
			}
		} else {
			files, err := ioutil.ReadDir(startPath)
			if err != nil {
				return err
			}

			for _, file := range files {
				s.checkAddSearchFile(file.Name())
			}
		}

	} else if fi.Mode().IsRegular() {
		s.checkAddSearchFile(s.Settings.StartPath)
	}

	s.addItemsDoneChan <- true

	return nil
}

func (s *Searcher) addSearchResult(r *SearchResult) {
	s.searchResults.AddSearchResult(r)
}

func linesMatch(lines []*string, inPatterns *SearchPatterns,
	outPatterns *SearchPatterns) bool {
	inLinesMatch := inPatterns.IsEmpty() || inPatterns.AnyMatchesAny(lines)
	outLinesMatch := !outPatterns.IsEmpty() && outPatterns.AnyMatchesAny(lines)
	return inLinesMatch && !outLinesMatch
}

func (s *Searcher) linesAfterMatch(linesAfter []*string) bool {
	return linesMatch(linesAfter, s.Settings.InLinesAfterPatterns,
		s.Settings.OutLinesAfterPatterns)
}

func (s *Searcher) linesBeforeMatch(linesBefore []*string) bool {
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

func splitIntoLines(bytes []byte) []*string {
	newlineidxs := newlineIndices(bytes)
	emptyStr := ""
	var lines []*string
	startidx, endidx := 0, 0
	for _, n := range newlineidxs {
		endidx = n
		if startidx == endidx {
			lines = append(lines, &emptyStr)
		} else if startidx < endidx {
			nextline := string(bytes[startidx:endidx])
			lines = append(lines, &nextline)
		}
		startidx = endidx + 1
	}
	endidx = len(bytes) - 1
	if bytes[endidx] == '\n' {
		lines = append(lines, &emptyStr)
	}
	return lines
}

func linesBeforeIndex(bytes []byte, idx int, lineCount int) []*string {
	var lines []*string
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

func linesAfterIndex(bytes []byte, idx int, lineCount int) []*string {
	var lines []*string
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

// public method to search a multi-line string
func (s *Searcher) SearchMultiLineString(str string) []*SearchResult {
	return s.searchTextBytes([]byte(str))
}

func (s *Searcher) searchTextBytes(bytes []byte) []*SearchResult {
	var results []*SearchResult
	var linesBefore []*string
	var linesAfter []*string
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
					&lineStr,
					linesBefore,
					linesAfter,
				}
				results = append(results, sr)

				// reset linesBefore and LinesAfter
				linesBefore, linesAfter = []*string{}, []*string{}
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
	var linesBefore []*string
	var linesAfter []*string
	linesAfterIdx := 0
	patternMatches := map[*regexp.Regexp]int{}
ReadLines:
	for {
		linenum++
		var line *string
		if len(linesAfter) > 0 {
			line, linesAfter = linesAfter[0], linesAfter[1:]
		} else if scanner.Scan() {
			text := scanner.Text()
			line = &text
		} else {
			break ReadLines
		}
		for len(linesAfter) < s.Settings.LinesAfter && scanner.Scan() {
			lineAfter := scanner.Text()
			linesAfter = append(linesAfter, &lineAfter)
		}
		spi := s.Settings.SearchPatterns.Iterator()
		for spi.Next() {
			p := spi.Value()
			if matchIndices := p.FindAllStringIndex(*line, -1); matchIndices != nil {
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
							linesAfter = append(linesAfter, &lineAfter)
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
				var srLinesAfter []*string
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
					&emptyStr,
					[]*string{},
					[]*string{},
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
			if s.isSearchFile(&file) {
				t := s.fileTypes.getFileType(file)
				newSearchItem := NewSearchItem(&dir, &file, t)
				for _, c := range si.Containers {
					newSearchItem.AddContainer(c)
				}
				newSearchItem.AddContainer(filepath.Join(*si.Path, *si.Name))
				s.searchFileReader(tr, newSearchItem)
			} else if s.isArchiveSearchFile(&file) {
				t := s.fileTypes.getFileType(file)
				newSearchItem := NewSearchItem(&dir, &file, t)
				for _, c := range si.Containers {
					newSearchItem.AddContainer(c)
				}
				newSearchItem.AddContainer(filepath.Join(*si.Path, *si.Name))
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
	if strings.HasSuffix(*si.Name, "tar.gz") || strings.HasSuffix(*si.Name, "tgz") {
		s.searchTarFileReader(gr, si)
	} else {
		name := gr.Name
		if s.isSearchFile(&name) {
			emptyStr := ""
			t := s.fileTypes.getFileType(name)
			newSearchItem := NewSearchItem(&emptyStr, &name, t)
			for _, c := range si.Containers {
				newSearchItem.AddContainer(c)
			}
			newSearchItem.AddContainer(filepath.Join(*si.Path, *si.Name))
			s.searchFileReader(gr, newSearchItem)
		}
	}
}

func (s *Searcher) searchBzip2FileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching bzip2 file %s", si.String()))
	}
	br := bzip2.NewReader(r)
	if strings.HasSuffix(*si.Name, "tar.bz2") {
		s.searchTarFileReader(br, si)
	} else {
		containedFileName := strings.TrimSuffix(*si.Name, ".bz2")
		if s.isSearchFile(&containedFileName) {
			emptyStr := ""
			t := s.fileTypes.getFileType(containedFileName)
			newSearchItem := NewSearchItem(&emptyStr, &containedFileName, t)
			for _, c := range si.Containers {
				newSearchItem.AddContainer(c)
			}
			newSearchItem.AddContainer(filepath.Join(*si.Path, *si.Name))
			s.searchFileReader(br, newSearchItem)
		}
	}
}

func (s *Searcher) searchZipFileReader(r io.Reader, si *SearchItem) {
	if s.Settings.Verbose {
		log(fmt.Sprintf("Searching zip file %s", si.String()))
	}
	// zip.OpenReader returns a *zip.ReaderCloser struct type that extends Reader
	zr, err := zip.OpenReader(filepath.Join(*si.Path, *si.Name))
	if err != nil {
		s.errChan <- err
		return
	}
	defer zr.Close()
	// f is a zip.File struct type
	for _, f := range zr.File {
		dir, file := filepath.Split(f.Name)
		if f.FileHeader.Flags != 0 && f.FileHeader.Flags != 2 {
			log(fmt.Sprintf("%s is an UNKNOWN file type", file))
		}
		// f.FileHeader.Flags == 2 seems to mean it's a file (not a dir, etc.)
		if f.FileHeader.Flags == 2 && s.isSearchFile(&file) {
			cr, err := f.Open()
			if err != nil {
				s.errChan <- err
				return
			}
			t := s.fileTypes.getFileType(file)
			newSearchItem := NewSearchItem(&dir, &file, t)
			for _, c := range si.Containers {
				newSearchItem.AddContainer(c)
			}
			newSearchItem.AddContainer(filepath.Join(*si.Path, *si.Name))
			s.searchFileReader(cr, newSearchItem)
			cr.Close()
		}
	}
}

func (s *Searcher) searchArchiveFileReader(r io.Reader, si *SearchItem) {
	if !s.isArchiveSearchFile(si.Name) {
		return
	}
	ext := getExtension(*si.Name)
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
	case FILETYPE_CODE, FILETYPE_XML, FILETYPE_TEXT:
		s.searchTextFileReader(r, si)
	case FILETYPE_BINARY:
		s.searchBinaryFileReader(r, si)
	case FILETYPE_ARCHIVE:
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
		if contains(s.Settings.InExtensions, getExtension(*si.Name)) {
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
		fullName := filepath.Join(*si.Path, *si.Name)
		r, err := os.Open(fullName)
		if err != nil {
			s.errChan <- err
			return
		}
		defer r.Close()
		s.searchFileReader(r, si)
		s.doneChan <- &fullName
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
	var searchItems []*SearchItem
	sii := s.searchItems.Iterator()
	for sii.Next() {
		searchItems = append(searchItems, sii.Value())
	}
	// start the processSearchChannels goroutine
	go s.processSearchChannels()

	// batch search (to avoid too many files open at once)
	for len(searchItems) > batchSize && len(s.errors) == 0 {
		s.doBatchFileSearch(searchItems[:batchSize])
		searchItems = searchItems[batchSize:]
	}
	if len(searchItems) > 0 && len(s.errors) == 0 {
		s.doBatchFileSearch(searchItems)
	}
	if len(s.errors) > 0 {
		return s.errors[0]
	}
	return nil
}

func (s *Searcher) processSearchChannels() {
	//get the results from the results channel
	doneFiles := map[string]bool{}
	for len(doneFiles) < s.searchItems.Count() && len(s.errors) == 0 {
		select {
		case r := <-s.resultChan:
			s.addSearchResult(r)
		case f := <-s.doneChan:
			doneFiles[*f] = true
		case e := <-s.errChan:
			s.errors = append(s.errors, e)
		}
	}
}

// the public-facing method takes a string and converts to a SearchItem type
func (s *Searcher) SearchFile(fp string) {
	dir, file := filepath.Split(fp)
	t := s.fileTypes.getFileType(file)
	searchItem := NewSearchItem(&dir, &file, t)
	s.searchSearchItem(searchItem)
}

//get the search items (files) from the file channel
func (s *Searcher) processSearchItemChannels() {
	addItemsDone := false
	for !addItemsDone {
		select {
		case b := <-s.addItemsDoneChan:
			addItemsDone = b
		case i := <-s.addItemChan:
			s.searchItems.AddItem(i)
		}
	}
}

func (s *Searcher) printToBeSearched() {
	dirs := []string{}
	files := []string{}

	sfi := s.searchItems.Iterator()
	for sfi.Next() {
		dirs = append(dirs, *sfi.Value().Path)
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
	// first start the processSearchItemChannels goroutine
	go s.processSearchItemChannels()

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
