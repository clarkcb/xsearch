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
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"

	"gofind/pkg/gofind"

	"golang.org/x/text/encoding"
	"golang.org/x/text/encoding/ianaindex"
	"golang.org/x/text/transform"
)

type Searcher struct {
	Finder             *gofind.Finder
	Settings           *SearchSettings
	searchDirs         []string
	fileResults        *gofind.FileResults
	errors             []error
	fileSearchedChan   chan string
	errChan            chan error
	addSearchFilesDone bool
	//searchDone         bool
	searchResults      *SearchResults
	addResultChan      chan *SearchResult
	addResultsDoneChan chan bool
	searchDoneChan     chan bool
	textDecoder        *encoding.Decoder
}

func NewSearcher(settings *SearchSettings) *Searcher {
	enc, err := ianaindex.IANA.Encoding(settings.TextFileEncoding())
	var textDecoder *encoding.Decoder = nil
	if err == nil {
		textDecoder = enc.NewDecoder()
	}
	return &Searcher{
		gofind.NewFinder(settings.FindSettings),
		settings,                // Settings
		[]string{},              // searchDirs
		gofind.NewFileResults(), // fileResults
		[]error{},               // errors
		make(chan string, 1),    // fileSearchedChan
		make(chan error, 1),     // errChan
		false,                   // addSearchFilesDone
		//false,                      // searchDone
		NewSearchResults(settings), // searchResults
		make(chan *SearchResult),   // addResultChan
		make(chan bool),            // addResultsDoneChan
		make(chan bool),            // searchDoneChan
		textDecoder,
	}
}

func (s *Searcher) ClearSearchResults() {
	s.searchResults = nil
}

func (s *Searcher) GetSearchResults() *SearchResults {
	return s.searchResults
}

const (
	NoSearchPatternsDefined = "No search patterns defined"
	InvalidLinesAfter       = "Invalid linesafter"
	InvalidLinesBefore      = "Invalid linesbefore"
	InvalidMaxLineLength    = "Invalid maxlinelength"
	InvalidTextFileEncoding = "Invalid or unsupported text file encoding"
)

func (s *Searcher) validateSettings() error {
	if s.Settings.SearchPatterns().IsEmpty() {
		return fmt.Errorf(NoSearchPatternsDefined)
	}
	if s.Settings.LinesAfter() < 0 {
		return fmt.Errorf(InvalidLinesAfter)
	}
	if s.Settings.LinesBefore() < 0 {
		return fmt.Errorf(InvalidLinesBefore)
	}
	if s.Settings.MaxLineLength() < 0 {
		return fmt.Errorf(InvalidMaxLineLength)
	}
	enc, err := ianaindex.IANA.Encoding(s.Settings.TextFileEncoding())
	if err != nil {
		return fmt.Errorf(InvalidTextFileEncoding)
	}
	s.textDecoder = enc.NewDecoder()
	return nil
}

func (s *Searcher) addSearchResult(r *SearchResult) {
	s.searchResults.AddSearchResult(r)
}

func linesMatch(lines []string, inPatterns *gofind.Patterns,
	outPatterns *gofind.Patterns) bool {
	inLinesMatch := inPatterns.IsEmpty() || inPatterns.AnyMatchesAny(lines)
	outLinesMatch := !outPatterns.IsEmpty() && outPatterns.AnyMatchesAny(lines)
	return inLinesMatch && !outLinesMatch
}

func (s *Searcher) linesAfterMatch(linesAfter []string) bool {
	return linesMatch(linesAfter, s.Settings.InLinesAfterPatterns(),
		s.Settings.OutLinesAfterPatterns())
}

func (s *Searcher) linesBeforeMatch(linesBefore []string) bool {
	return linesMatch(linesBefore, s.Settings.InLinesBeforePatterns(),
		s.Settings.OutLinesBeforePatterns())
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

func (s *Searcher) searchTextFileReaderContents(r io.Reader, fr *gofind.FileResult) {
	bytes, err := io.ReadAll(r)
	if err != nil {
		s.errChan <- err
		return
	}
	results := s.searchTextBytes(bytes)
	for _, sr := range results {
		sr.File = fr
		s.addResultChan <- sr
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
	if s.Settings.FirstMatch() {
		findLimit = 1
	}
	spi := s.Settings.SearchPatterns().Iterator()
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
				if s.Settings.LinesBefore() > 0 && beforeLineCount > 0 {
					linesBefore = linesBeforeIndex(bytes, startidx, s.Settings.LinesBefore())
				}
				if s.Settings.LinesAfter() > 0 && afterLineCount > 0 {
					linesAfter = linesAfterIndex(bytes, endidx, s.Settings.LinesAfter())
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

func (s *Searcher) searchTextFileReaderLines(r io.Reader, fr *gofind.FileResult) {
	results := s.SearchTextReaderLines(r)
	for _, sr := range results {
		sr.File = fr
		s.addResultChan <- sr
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
		for len(linesAfter) < s.Settings.LinesAfter() && scanner.Scan() {
			lineAfter := scanner.Text()
			linesAfter = append(linesAfter, lineAfter)
		}
		spi := s.Settings.SearchPatterns().Iterator()
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
				if !s.Settings.LinesAfterToPatterns().IsEmpty() ||
					!s.Settings.LinesAfterUntilPatterns().IsEmpty() {

					if !s.Settings.LinesAfterToPatterns().IsEmpty() &&
						s.Settings.LinesAfterToPatterns().AnyMatchesAny(linesAfter) {
						linesAfterToMatch = true
					}
					if !s.Settings.LinesAfterUntilPatterns().IsEmpty() &&
						s.Settings.LinesAfterUntilPatterns().AnyMatchesAny(linesAfter) {
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
						if !s.Settings.LinesAfterToPatterns().IsEmpty() &&
							s.Settings.LinesAfterToPatterns().MatchesAny(nextLine) {
							linesAfterToMatch = true
						}
						if !s.Settings.LinesAfterUntilPatterns().IsEmpty() &&
							s.Settings.LinesAfterUntilPatterns().MatchesAny(nextLine) {
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
					if !s.Settings.LinesAfterToPatterns().IsEmpty() {
						srLinesAfter = linesAfter[:lastIdx]
					} else if !s.Settings.LinesAfterUntilPatterns().IsEmpty() {
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
					if s.Settings.FirstMatch() && patternMatched {
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
		if s.Settings.LinesBefore() > 0 {
			if len(linesBefore) == s.Settings.LinesBefore() {
				linesBefore = linesBefore[1:]
			}
			if len(linesBefore) < s.Settings.LinesBefore() {
				linesBefore = append(linesBefore, line)
			}
		}
	}
	if err := scanner.Err(); err != nil {
		s.errChan <- err
	}
	return results
}

func (s *Searcher) searchTextFileReader(r io.Reader, fr *gofind.FileResult) {
	if s.Settings.Verbose() {
		gofind.Log(fmt.Sprintf("Searching text file %s", fr.String()))
	}
	if s.Settings.MultiLineSearch() {
		s.searchTextFileReaderContents(r, fr)
	} else {
		s.searchTextFileReaderLines(r, fr)
	}
}

func (s *Searcher) searchBinaryFileReader(r io.Reader, fr *gofind.FileResult) {
	if s.Settings.Verbose() {
		gofind.Log(fmt.Sprintf("Searching binary file %s", fr.String()))
	}
	bytes, err := io.ReadAll(r)
	if err != nil {
		s.errChan <- err
		return
	}
	findLimit := -1
	if s.Settings.FirstMatch() {
		findLimit = 1
	}
	spi := s.Settings.SearchPatterns().Iterator()
	for spi.Next() {
		p := spi.Value()
		if matchIndices := p.FindAllIndex(bytes, findLimit); matchIndices != nil {
			for _, m := range matchIndices {
				emptyStr := ""
				sr := &SearchResult{
					p,
					fr,
					0,
					m[0] + 1,
					m[1] + 1,
					emptyStr,
					[]string{},
					[]string{},
				}
				s.addResultChan <- sr
			}
		}
	}
}

func notR(c rune) bool {
	return c != 'r'
}

func (s *Searcher) searchTarFileReader(r io.Reader, fr *gofind.FileResult) {
	if s.Settings.Verbose() {
		tarName := strings.TrimRightFunc(fr.String(), notR)
		gofind.Log(fmt.Sprintf("Searching tar file %s", tarName))
	}
	tr := tar.NewReader(r)
	for {
		//hdr, err := tr.Next()
		_, err := tr.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			if err == io.ErrUnexpectedEOF {
				if s.Settings.Debug() {
					gofind.Log(fmt.Sprintf("Encountered unexpected EOF in tar file %s",
						fr.String()))
				}
				break
			}
			if s.Settings.Debug() {
				gofind.Log(fmt.Sprintf("Error encountered in searchTarFileReader: %s",
					err))
			}
			s.errChan <- err
		}
		// TODO: modify xfind to pre-find all matching files in archives
		//dir, file := filepath.Split(hdr.Name)
		//if !strings.HasSuffix(hdr.Name, "/") {
		//	if s.isSearchFile(file) {
		//		t := s.fileTypes.getFileType(file)
		//		newSearchItem := NewSearchItem(dir, file, t)
		//		for _, c := range fr.Containers {
		//			newSearchItem.AddContainer(c)
		//		}
		//		newSearchItem.AddContainer(filepath.Join(fr.Path, fr.Name))
		//		s.searchFileReader(tr, newSearchItem)
		//	} else if s.isArchiveSearchFile(file) {
		//		t := s.fileTypes.getFileType(file)
		//		newSearchItem := NewSearchItem(dir, file, t)
		//		for _, c := range fr.Containers {
		//			newSearchItem.AddContainer(c)
		//		}
		//		newSearchItem.AddContainer(filepath.Join(fr.Path, fr.Name))
		//		s.searchArchiveFileReader(tr, newSearchItem)
		//	}
		//}
	}
}

func (s *Searcher) searchGzipFileReader(r io.Reader, fr *gofind.FileResult) {
	if s.Settings.Verbose() {
		gofind.Log(fmt.Sprintf("Searching gzip file %s", fr.String()))
	}
	gr, err := gzip.NewReader(r)
	if err != nil {
		if s.Settings.Debug() {
			gofind.Log(fmt.Sprintf("Error encountered in searchGzipFileReader: %s",
				err))
		}
		s.errChan <- err
		return
	}
	defer func() {
		gr.Close()
	}()
	// TODO: modify xfind to pre-find all matching files in archives
	//if strings.HasSuffix(fr.Name, "tar.gz") || strings.HasSuffix(fr.Name, "tgz") {
	//	s.searchTarFileReader(gr, fr)
	//} else {
	//	name := gr.Name
	//	if s.isSearchFile(name) {
	//		emptyStr := ""
	//		t := s.fileTypes.getFileType(name)
	//		newSearchItem := NewSearchItem(emptyStr, name, t)
	//		for _, c := range fr.Containers {
	//			newSearchItem.AddContainer(c)
	//		}
	//		newSearchItem.AddContainer(filepath.Join(fr.Path, fr.Name))
	//		s.searchFileReader(gr, newSearchItem)
	//	}
	//}
}

func (s *Searcher) searchBzip2FileReader(r io.Reader, fr *gofind.FileResult) {
	if s.Settings.Verbose() {
		gofind.Log(fmt.Sprintf("Searching bzip2 file %s", fr.String()))
	}
	br := bzip2.NewReader(r)
	if strings.HasSuffix(fr.Name, "tar.bz2") {
		s.searchTarFileReader(br, fr)
	} else {
		// TODO: modify xfind to pre-find all matching files in archives
		//containedFileName := strings.TrimSuffix(fr.Name, ".bz2")
		//if s.isSearchFile(containedFileName) {
		//	emptyStr := ""
		//	t := s.fileTypes.getFileType(containedFileName)
		//	newSearchItem := NewSearchItem(emptyStr, containedFileName, t)
		//	for _, c := range fr.Containers {
		//		newSearchItem.AddContainer(c)
		//	}
		//	newSearchItem.AddContainer(filepath.Join(fr.Path, fr.Name))
		//	s.searchFileReader(br, newSearchItem)
		//}
	}
}

func (s *Searcher) searchZipFileReader(r io.Reader, fr *gofind.FileResult) {
	if s.Settings.Verbose() {
		gofind.Log(fmt.Sprintf("Searching zip file %s", fr.String()))
	}
	// zip.OpenReader returns a *zip.ReaderCloser struct type that extends Reader
	zr, err := zip.OpenReader(filepath.Join(fr.Path, fr.Name))
	if err != nil {
		s.errChan <- err
		return
	}
	defer func(zr *zip.ReadCloser) {
		err := zr.Close()
		if err != nil {

		}
	}(zr)
	// TODO: modify xfind to pre-find all matching files in archives
	// f is a zip.File struct type
	//for _, f := range zr.File {
	//	dir, file := filepath.Split(f.Name)
	//	if f.FileHeader.Flags != 0 && f.FileHeader.Flags != 2 {
	//		gofind.Log(fmt.Sprintf("%s is an UNKNOWN file type", file))
	//	}
	//	// f.FileHeader.Flags == 2 seems to mean it's a file (not a dir, etc.)
	//	if f.FileHeader.Flags == 2 && s.isSearchFile(file) {
	//		cr, err := f.Open()
	//		if err != nil {
	//			s.errChan <- err
	//			return
	//		}
	//		t := s.fileTypes.getFileType(file)
	//		newSearchItem := NewSearchItem(dir, file, t)
	//		for _, c := range fr.Containers {
	//			newSearchItem.AddContainer(c)
	//		}
	//		newSearchItem.AddContainer(filepath.Join(fr.Path, fr.Name))
	//		s.searchFileReader(cr, newSearchItem)
	//		cr.Close()
	//	}
	//}
}

func (s *Searcher) searchArchiveFileReader(r io.Reader, fr *gofind.FileResult) {
	ext := gofind.GetExtension(fr.Name)
	switch ext {
	case "zip", "jar", "war", "ear":
		s.searchZipFileReader(r, fr)
	case "gz", "tgz":
		s.searchGzipFileReader(r, fr)
	case "bz2":
		s.searchBzip2FileReader(r, fr)
	default:
		gofind.Log(fmt.Sprintf("Searching not currently supported for %s files", ext))
	}
}

func (s *Searcher) searchFileReader(r io.Reader, fr *gofind.FileResult) {
	switch fr.FileType {
	case gofind.FileTypeCode, gofind.FileTypeXml, gofind.FileTypeText:
		s.searchTextFileReader(transform.NewReader(r, s.textDecoder), fr)
	case gofind.FileTypeBinary:
		s.searchBinaryFileReader(r, fr)
	case gofind.FileTypeArchive:
		if s.Settings.SearchArchives() {
			s.searchArchiveFileReader(r, fr)
		} else {
			if s.Settings.Verbose() {
				gofind.Log(fmt.Sprintf("Skipping archive file: %s", fr.String()))
			}
		}
	default:
		gofind.Log(fmt.Sprintf("Skipping unknown file type: %s", fr.String()))
	}
}

func (s *Searcher) searchFileResult(fr *gofind.FileResult) {
	if fr.FileType == gofind.FileTypeUnknown {
		if gofind.Contains(s.Settings.InExtensions(), gofind.GetExtension(fr.Name)) {
			if s.Settings.Debug() {
				gofind.Log(fmt.Sprintf("File made searchable by passing in-ext: %s",
					fr.String()))
			}
		} else {
			if s.Settings.Verbose() {
				gofind.Log(fmt.Sprintf("Skipping unsearchable file: %s", fr.String()))
			}
			return
		}
	}
	// make sure it doesn't have any containers (not sure how this could happen)
	if len(fr.Containers) > 0 {
		gofind.Log(fmt.Sprintf("Has containers: %s", fr.String()))
	} else {
		// create an io.Reader
		fullName := filepath.Join(fr.Path, fr.Name)
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
		s.searchFileReader(r, fr)
		s.fileSearchedChan <- fullName
	}
}

// initiates goroutines to search each file in the batch, waiting for all
// to finish before returning
func (s *Searcher) batchSearchFiles(files []*gofind.FileResult) {
	wg := &sync.WaitGroup{}
	wg.Add(len(files)) // set the WaitGroup counter to files length
	for _, fr := range files {
		go func(wg *sync.WaitGroup, fr *gofind.FileResult) {
			s.searchFileResult(fr)
			wg.Done() // decrement the counter
		}(wg, fr)
	}
	wg.Wait()
}

func (s *Searcher) printToBeSearched(fileResults *gofind.FileResults) {
	var dirs []string
	var files []string

	fri := fileResults.Iterator()
	for fri.Next() {
		dirs = append(dirs, fri.Value().Path)
		files = append(files, fri.Value().String())
	}

	dirMap := gofind.MakeStringMap(dirs)
	dirs = gofind.GetSortedKeys(dirMap)

	gofind.Log(fmt.Sprintf("\nDirectories to be searched (%d):", len(dirs)))
	for _, d := range dirs {
		gofind.Log(d)
	}

	fileMap := gofind.MakeStringMap(files)
	files = gofind.GetSortedKeys(fileMap)

	gofind.Log(fmt.Sprintf("\nFiles to be searched (%d):", len(files)))
	for _, f := range files {
		gofind.Log(f)
	}
}

func (s *Searcher) activateSearchChannels() {
	// get the results from the results channel
	searchedFiles := map[string]bool{}
	searchingDone := false
	for !searchingDone {
		select {
		case r := <-s.addResultChan:
			s.addSearchResult(r)
		case f := <-s.fileSearchedChan:
			searchedFiles[f] = true
			if len(searchedFiles) == s.fileResults.Len() {
				s.searchDoneChan <- true
				searchingDone = true
			}
		case e := <-s.errChan:
			s.errors = append(s.errors, e)
			s.searchDoneChan <- true
			searchingDone = true
		}
	}
}

func (s *Searcher) searchFiles(fileResults *gofind.FileResults) error {
	const batchSize = 240 // max files to search at one time

	// start the activateSearchChannels goroutine
	go s.activateSearchChannels()

	fri := fileResults.Iterator()
	var files []*gofind.FileResult
	for fri.Next() && len(s.errors) == 0 {
		files = fri.Take(batchSize)
		s.batchSearchFiles(files)
	}

	//searchDone := false
	//for !searchDone {
	//	select {
	//	case b := <-s.searchDoneChan:
	//		searchDone = b
	//	}
	//}

	if len(s.errors) > 0 {
		return s.errors[0]
	}
	return nil
}

func (s *Searcher) Search() (*SearchResults, error) {
	if err := s.Settings.Validate(); err != nil {
		return nil, err
	}

	// get search file list + validate find settings
	var err error
	s.searchResults.FileResults, err = s.Finder.Find()

	if err != nil {
		return nil, err
	}

	if s.Settings.Verbose() {
		s.printToBeSearched(s.searchResults.FileResults)
	}

	// search the files
	if s.Settings.Verbose() {
		gofind.Log("\nStarting file search...\n")
	}
	if err := s.searchFiles(s.searchResults.FileResults); err != nil {
		return nil, err
	}
	if s.Settings.Verbose() {
		gofind.Log("\nFile search complete.\n")
	}

	// sort the SearchResults
	s.searchResults.Sort(s.Settings)

	return s.searchResults, nil
}
