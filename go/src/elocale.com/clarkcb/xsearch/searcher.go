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
	"time"
)

type Searcher struct {
	Settings      *SearchSettings
	fileTypes     *FileTypes
	searchDirs    []*string
	searchItems   *SearchItems
	addItemChan   chan *SearchItem
	doneChan      chan *string
	errChan       chan error
	searchResults *SearchResults
	resultChan    chan *SearchResult
	timerMap      map[string]time.Time
}

func NewSearcher(settings *SearchSettings) *Searcher {
	return &Searcher{
		settings,                 // Settings
		GetFileTypes(),           // fileTypes
		[]*string{},              // searchDirs
		NewSearchItems(),         // searchItems
		make(chan *SearchItem),   // addItemChan
		make(chan *string),       // doneChan
		make(chan error),         // errChan
		NewSearchResults(),       // searchResults
		make(chan *SearchResult), // resultChan
		map[string]time.Time{},   // timerMap
	}
}

func (s *Searcher) ClearSearchResults() {
	s.searchResults = nil
}

func (s *Searcher) GetSearchResults() *SearchResults {
	return s.searchResults
}

func (s *Searcher) validSettings() error {
	if s.Settings.StartPath == "" {
		return fmt.Errorf("Missing startpath")
	}
	if _, err := os.Stat(s.Settings.StartPath); err != nil {
		return fmt.Errorf("Invalid startpath: \"%s\"", s.Settings.StartPath)
	}
	if s.Settings.SearchPatterns.IsEmpty() {
		return fmt.Errorf("No search patterns defined")
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
	if *d != "." && *d != ".." {
		if strings.HasPrefix(*d, ".") && s.Settings.ExcludeHidden {
			return false
		}
	}
	return filterInBySearchPatterns(d, s.Settings.InDirPatterns,
		s.Settings.OutDirPatterns)
}

func (s *Searcher) addSearchDir(d *string) {
	s.searchDirs = append(s.searchDirs, d)
}

func (s *Searcher) checkAddSearchDir(d *string) error {
	if s.isSearchDir(d) {
		s.addSearchDir(d)
	}
	return nil
}

// this method passed to the filepath.Walk method, it must have this signature
func (s *Searcher) checkAddSearchWalkDir(d string, fi os.FileInfo, err error) error {
	if fi.IsDir() {
		return s.checkAddSearchDir(&d)
	}
	return nil
}

func (s *Searcher) setSearchDirs() error {
	if s.Settings.Verbose {
		log("\nBuilding directory search list")
	}
	if s.Settings.Recursive {
		return filepath.Walk(s.Settings.StartPath, s.checkAddSearchWalkDir)
	}
	return s.checkAddSearchDir(&s.Settings.StartPath)
}

func (s *Searcher) isArchiveSearchFile(filename *string) bool {
	if s.fileTypes.IsArchiveFile(*filename) {

		if strings.HasPrefix(*filename, ".") && s.Settings.ExcludeHidden {
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

func (s *Searcher) isSearchFile(filename *string) bool {
	if strings.HasPrefix(*filename, ".") && s.Settings.ExcludeHidden {
		return false
	}
	ext := getExtension(*filename)
	if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
		return false
	}
	if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
		return false
	}
	return filterInBySearchPatterns(filename, s.Settings.InFilePatterns,
		s.Settings.OutFilePatterns)
}

func (s *Searcher) setSearchFilesForDirectory(d *string) {
	fileInfos, err := ioutil.ReadDir(*d)
	if err != nil {
		if s.Settings.Debug {
			log(fmt.Sprintf("err in setSearchFilesForDirectory: %s\n", err.Error()))
		}
		s.errChan <- err
		return
	}
	for _, fi := range fileInfos {
		if !fi.IsDir() {
			name := fi.Name()
			if s.Settings.SearchArchives && s.isArchiveSearchFile(&name) {
				// this approach was too slow, reverting to adding archive file itself
				//s.setArchiveSearchFiles(filepath.Join(d, name))
				s.addItemChan <- NewSearchItem(d, &name)
			} else if !s.Settings.ArchivesOnly && s.isSearchFile(&name) {
				s.addItemChan <- NewSearchItem(d, &name)
			}
		}
	}
	s.doneChan <- d
}

func (s *Searcher) setSearchFiles() {
	if s.Settings.Verbose {
		log("\nBuilding file search list")
	}

	// gather search files concurrently for each search dir
	for _, d := range s.searchDirs {
		go func(dir *string) {
			s.setSearchFilesForDirectory(dir)
		}(d)
	}
}

func (s *Searcher) addSearchResult(r *SearchResult) {
	if s.Settings.UniqueLines &&
		s.searchResults.LineCounts[strings.TrimSpace(*r.Line)] > 0 {
		if s.Settings.Verbose {
			log(fmt.Sprintf("Skipping search result with non-unique line: %s",
				strings.TrimSpace(*r.Line)))
		}
	} else {
		s.searchResults.AddSearchResult(r)
	}
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
	newlineidxs := []int{}
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
	lines := []*string{}
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
	lines := []*string{}
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
	lines := []*string{}
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
	results := []*SearchResult{}
	linesBefore := []*string{}
	linesAfter := []*string{}
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
	results := []*SearchResult{}
	scanner := bufio.NewScanner(r)
	linenum := 0
	linesBefore := []*string{}
	linesAfter := []*string{}
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

					for ; !linesAfterToMatch && !linesAfterUntilMatch;  {
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
					_,patternMatched := patternMatches[p]
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
	spi := s.Settings.SearchPatterns.Iterator()
	for spi.Next() {
		p := spi.Value()
		if p.Match(bytes) {
			emptyStr := ""
			sr := &SearchResult{
				p,
				si,
				0,
				0,
				0,
				&emptyStr,
				[]*string{},
				[]*string{},
			}
			s.resultChan <- sr
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
				newSearchItem := NewSearchItem(&dir, &file)
				for _, c := range si.Containers {
					newSearchItem.AddContainer(c)
				}
				newSearchItem.AddContainer(filepath.Join(*si.Path, *si.Name))
				s.searchFileReader(tr, newSearchItem)
			} else if s.isArchiveSearchFile(&file) {
				newSearchItem := NewSearchItem(&dir, &file)
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
			newSearchItem := NewSearchItem(&emptyStr, &name)
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
			newSearchItem := NewSearchItem(&emptyStr, &containedFileName)
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
			newSearchItem := NewSearchItem(&dir, &file)
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
	switch s.fileTypes.getFileType(*si.Name) {
	case FILETYPE_TEXT:
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
	if !s.fileTypes.IsSearchableFile(*si.Name) {
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

func (s *Searcher) addTimer(name string, action string) {
	timerName := fmt.Sprintf("%s:%s", name, action)
	s.timerMap[timerName] = time.Now()
}

func (s *Searcher) startTimer(name string) {
	s.addTimer(name, "start")
}

func (s *Searcher) stopTimer(name string) {
	s.addTimer(name, "stop")
	if s.Settings.PrintResults || s.Settings.Debug {
		s.printElapsed(name)
	}
}

func (s *Searcher) getElapsed(name string) time.Duration {
	start := s.timerMap[name+":start"]
	stop := s.timerMap[name+":stop"]
	return stop.Sub(start)
}

func (s *Searcher) printElapsed(name string) {
	elapsed := s.getElapsed(name)
	log(fmt.Sprintf("Elapsed time for %s: %v\n", name, elapsed))
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

func (s *Searcher) doFileSearch() {
	const batchSize = 240 // max files to search at one time
	searchItems := []*SearchItem{}
	sii := s.searchItems.Iterator()
	for sii.Next() {
		searchItems = append(searchItems, sii.Value())
	}
	// start the processSearchChannels goroutine
	go s.processSearchChannels()

	// batch search (to avoid too many files open at once)
	for len(searchItems) > batchSize {
		nextSearchItems := searchItems[:batchSize]
		s.doBatchFileSearch(nextSearchItems)
		searchItems = searchItems[batchSize:]
	}
	s.doBatchFileSearch(searchItems)
}

func (s *Searcher) processSearchChannels() {
	//get the results from the results channel
	doneFiles := map[string]bool{}
	for len(doneFiles) < s.searchItems.Count() {
		select {
		case r := <-s.resultChan:
			s.addSearchResult(r)
		case f := <-s.doneChan:
			doneFiles[*f] = true
		case e := <-s.errChan:
			panic(e)
		}
	}
}

// the public-facing method takes a string and converts to a SearchItem type
func (s *Searcher) SearchFile(fp string) {
	dir, file := filepath.Split(fp)
	searchItem := NewSearchItem(&dir, &file)
	s.searchSearchItem(searchItem)
}

func (s *Searcher) Search() error {
	if err := s.validSettings(); err != nil {
		return err
	}

	// get search directory list
	if s.Settings.DoTiming {
		s.startTimer("setSearchDirs")
	}
	err := s.setSearchDirs()
	if s.Settings.DoTiming {
		s.stopTimer("setSearchDirs")
	}
	if err != nil {
		return err
	}

	if s.Settings.Verbose {
		log(fmt.Sprintf("\nDirectories to be searched (%d):", len(s.searchDirs)))
		for _, d := range s.searchDirs {
			log(*d)
		}
	}

	// get search file list
	if s.Settings.DoTiming {
		s.startTimer("setSearchFiles")
	}
	s.setSearchFiles()

	//get the search items (files) from the file channel
	doneDirs := map[string]bool{}
	for len(doneDirs) < len(s.searchDirs) {
		select {
		case i := <-s.addItemChan:
			s.searchItems.AddItem(i)
		case d := <-s.doneChan:
			doneDirs[*d] = true
		case e := <-s.errChan:
			panic(e)
		}
	}
	if s.Settings.DoTiming {
		s.stopTimer("setSearchFiles")
	}

	if s.Settings.Verbose {
		log(fmt.Sprintf("\nFiles to be searched (%d):", s.searchItems.Count()))
		sfi := s.searchItems.Iterator()
		for sfi.Next() {
			log(sfi.Value().String())
		}
	}

	// search the files
	if s.Settings.Verbose {
		log("\nStarting file search...\n")
	}
	if s.Settings.DoTiming {
		s.startTimer("searchFiles")
	}
	s.doFileSearch()

	if s.Settings.DoTiming {
		s.stopTimer("searchFiles")
	}
	if s.Settings.Verbose {
		log("\nFile search complete.\n")
	}

	return nil
}

func (s *Searcher) PrintSearchResults() {
	s.searchResults.PrintSearchResults()
	log("")
	patternKeys := []string{}

	spi := s.Settings.SearchPatterns.Iterator()
	for spi.Next() {
		p := spi.Value()
		patternKeys = append(patternKeys, p.String())
	}
	s.searchResults.PrintPatternCounts(patternKeys)
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
