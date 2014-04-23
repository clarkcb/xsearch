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
	"time"
)

type Searcher struct {
	Settings      *SearchSettings
	fileUtil      *FileUtil
	searchDirs    []string
	searchFiles   []string
	searchResults *SearchResults
	timerMap      map[string]time.Time
}

func NewSearcher(settings *SearchSettings) *Searcher {
	return &Searcher{
		settings,
		NewFileUtil(),
		[]string{},
		[]string{},
		NewSearchResults(),
		map[string]time.Time{},
	}
}

func (s *Searcher) validSettings() error {
	if s.Settings.StartPath == "" {
		return fmt.Errorf("Missing startpath")
	}
	if _, err := os.Stat(s.Settings.StartPath); err != nil {
		return fmt.Errorf("Invalid startpath")
	}
	if len(s.Settings.SearchPatterns) == 0 {
		return fmt.Errorf("No search patterns defined")
	}
	return nil
}

func (s *Searcher) testFileTypeChecking() {
	// test file type checking
	testFiles := []string{"hello.go", "cmp.zip", "runme.exe", "blabla.bla"}
	for _, f := range testFiles {
		fmt.Printf("IsSearchableFile(%s): %t\n", f,
			s.fileUtil.IsSearchableFile(f))
		fmt.Printf("IsTextFile(%s): %t\n", f,
			s.fileUtil.IsTextFile(f))
		fmt.Printf("IsBinaryFile(%s): %t\n", f,
			s.fileUtil.IsBinaryFile(f))
		fmt.Printf("IsCompressedFile(%s): %t\n", f,
			s.fileUtil.IsCompressedFile(f))
	}
}

func (s *Searcher) isSearchDir(path string) bool {
	//return true
	if len(s.Settings.InDirPatterns) > 0 && !matchesAnyPattern(path, &s.Settings.InDirPatterns) {
		return false
	}
	if len(s.Settings.OutDirPatterns) > 0 && matchesAnyPattern(path, &s.Settings.OutDirPatterns) {
		return false
	}
	return true
}

func (s *Searcher) checkAddSearchDir(path string, fi os.FileInfo, err error) error {
	if fi.IsDir() && s.isSearchDir(path) {
		s.searchDirs = append(s.searchDirs, path)
	}
	return nil
}

func (s *Searcher) setSearchDirs() error {
	if s.Settings.Verbose {
		fmt.Println("\nBuilding directory search list")
	}
	return filepath.Walk(s.Settings.StartPath, s.checkAddSearchDir)
}

func (s *Searcher) isSearchFile(filename string) bool {
	if s.Settings.SearchCompressed && s.fileUtil.IsCompressedFile(filename) {
		return true
	}
	ext := s.fileUtil.getExtension(filename)
	if len(s.Settings.InExtensions) > 0 && !contains(s.Settings.InExtensions, ext) {
		return false
	}
	if len(s.Settings.OutExtensions) > 0 && contains(s.Settings.OutExtensions, ext) {
		return false
	}
	if len(s.Settings.InFilePatterns) > 0 && !matchesAnyPattern(filename, &s.Settings.InFilePatterns) {
		return false
	}
	if len(s.Settings.OutFilePatterns) > 0 && matchesAnyPattern(filename, &s.Settings.OutFilePatterns) {
		return false
	}
	return true
}

func (s *Searcher) setSearchFiles() error {
	if s.Settings.Verbose {
		fmt.Println("\nBuilding file search list")
	}
	for _, d := range s.searchDirs {
		fileInfos, err := ioutil.ReadDir(d)
		if err != nil {
			return err
		}
		for _, fi := range fileInfos {
			if !fi.IsDir() && s.isSearchFile(fi.Name()) {
				s.searchFiles = append(s.searchFiles, filepath.Join(d, fi.Name()))
			}
		}
	}
	return nil
}

func (s *Searcher) addSearchResult(r *SearchResult) {
	s.searchResults.AddSearchResult(r)
}

func linesMatch(lines *[]string, inPatterns *[]*regexp.Regexp,
	outPatterns *[]*regexp.Regexp) bool {
	inLinesMatch := len(*inPatterns) == 0 || anyMatchesAnyPattern(lines, inPatterns)
	outLinesMatch := len(*outPatterns) > 0 && anyMatchesAnyPattern(lines, outPatterns)
	return inLinesMatch && !outLinesMatch
}

func (s *Searcher) linesAfterMatch(linesAfter *[]string) bool {
	return linesMatch(linesAfter, &s.Settings.InLinesAfterPatterns,
		&s.Settings.OutLinesAfterPatterns)
}

func (s *Searcher) linesBeforeMatch(linesBefore *[]string) bool {
	return linesMatch(linesBefore, &s.Settings.InLinesBeforePatterns,
		&s.Settings.OutLinesBeforePatterns)
}

func (s *Searcher) searchTextFileReader(r io.Reader, filepath string) error {
	if s.Settings.Verbose {
		fmt.Printf("Searching text file %s\n", filepath)
	}
	if s.Settings.MultiLineSearch {
		return s.searchTextFileReaderContents(r, filepath)
	} else {
		return s.searchTextFileReaderLines(r, filepath)
	}
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

func splitIntoLines(bytes []byte) []string {
	fmt.Printf("splitIntoLines(bytes=%s)\n", string(bytes))
	newlineidxs := newlineIndices(bytes)
	fmt.Printf("newlineidxs: %v\n", newlineidxs)
	lines := []string{}
	startidx, endidx := 0, 0
	for _, n := range newlineidxs {
		endidx = n
		fmt.Printf("startidx: %d, endidx: %d\n", startidx, endidx)
		if startidx == endidx {
			lines = append(lines, "")
		} else if startidx < endidx {
			nextline := string(bytes[startidx:endidx])
			fmt.Printf("nextline: %s\n", nextline)
			lines = append(lines, nextline)
		}
		startidx = endidx + 1
	}
	endidx = len(bytes) - 1
	fmt.Printf("endidx: %d\n", endidx)
	if bytes[endidx] == '\n' {
		lines = append(lines, "")
	}
	return lines
}

func linesBeforeIndex(bytes []byte, idx int, lineCount int) []string {
	fmt.Printf("linesBeforeIndex(idx=%d, lineCount=%d)\n", idx, lineCount)
	fmt.Printf("bytes[%d]: %#U\n", idx, bytes[idx])
	lines := []string{}
	if idx < 1 {
		return lines
	}
	newlines := 0
	beforeidx := idx
	for beforeidx > 0 && newlines < lineCount {
		fmt.Printf("beforeidx: %d\n", beforeidx)
		fmt.Printf("bytes[%d]: %#U\n", beforeidx, bytes[beforeidx])
		if bytes[beforeidx] == '\n' {
			newlines++
			fmt.Printf("newlines: %d\n", newlines)
		}
		beforeidx--
	}
	beforestartlineidx, _ := lineStartEndIndicesForIndex(beforeidx, bytes)
	fmt.Printf("beforestartlineidx: %d\n", beforestartlineidx)
	lines = splitIntoLines(bytes[beforestartlineidx : idx-1])
	return lines
}

func linesAfterIndex(bytes []byte, idx int, lineCount int) []string {
	fmt.Printf("linesAfterIndex(idx=%d, lineCount=%d)\n", idx, lineCount)
	fmt.Printf("bytes[%d]: %#U\n", idx, bytes[idx])
	lines := []string{}
	newlines := 0
	afteridx := idx
	for afteridx < len(bytes)-1 && newlines < lineCount {
		fmt.Printf("afteridx: %d\n", afteridx)
		fmt.Printf("bytes[%d]: %#U\n", afteridx, bytes[afteridx])
		if bytes[afteridx] == '\n' {
			newlines++
			fmt.Printf("newlines: %d\n", newlines)
		}
		afteridx++
	}
	_, afterendlineidx := lineStartEndIndicesForIndex(afteridx-1, bytes)
	fmt.Printf("afterendlineidx: %d\n", afterendlineidx)
	lines = splitIntoLines(bytes[idx:afterendlineidx])
	return lines[:lineCount]
}

func (s *Searcher) searchTextFileReaderContents(r io.Reader, filepath string) error {
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}
	linesBefore := []string{}
	linesAfter := []string{}
	findLimit := -1
	if s.Settings.FirstMatch {
		findLimit = 1
	}
	for _, p := range s.Settings.SearchPatterns {
		allIndices := p.FindAllIndex(bytes, findLimit)
		if allIndices != nil {
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
				sr := &SearchResult{
					p, filepath, linenum,
					strings.TrimRight(string(line), "\r\n"),
					linesBefore,
					linesAfter,
				}
				s.addSearchResult(sr)
				// reset linesBefore and LinesAfter
				linesBefore, linesAfter = []string{}, []string{}
			}
		}
	}
	return nil
}

func (s *Searcher) searchTextFileReaderLines(r io.Reader, filepath string) error {
	scanner := bufio.NewScanner(r)
	linenum := 0
	linesBefore := []string{}
	linesAfter := []string{}
ReadLines:
	for {
		linenum++
		var line string
		if len(linesAfter) > 0 {
			line, linesAfter = linesAfter[0], linesAfter[1:]
		} else if scanner.Scan() {
			line = scanner.Text()
		} else {
			break ReadLines
		}
		for len(linesAfter) < s.Settings.LinesAfter && scanner.Scan() {
			linesAfter = append(linesAfter, scanner.Text())
		}
		for _, p := range s.Settings.SearchPatterns {
			// check for FirstMatch setting and stop if file+pattern match exists
			if s.Settings.FirstMatch &&
				s.searchResults.HasResultForFileAndPattern(filepath, p) {
				if len(s.Settings.SearchPatterns) > 1 {
					continue
				} else {
					break ReadLines
				}
			}
			if p.MatchString(line) {
				if len(linesBefore) > 0 && !s.linesBeforeMatch(&linesBefore) {
					continue
				} else if len(linesAfter) > 0 && !s.linesAfterMatch(&linesAfter) {
					continue
				} else {
					sr := &SearchResult{
						p, filepath, linenum, line,
						linesBefore,
						linesAfter,
					}
					s.addSearchResult(sr)
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
		return err
	}
	return nil
}

func (s *Searcher) searchBinaryFileReader(r io.Reader, filepath string) error {
	if s.Settings.Verbose {
		fmt.Printf("Searching binary file %s\n", filepath)
	}
	bytes, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}
	for _, p := range s.Settings.SearchPatterns {
		if p.Match(bytes) {
			sr := &SearchResult{
				p, filepath, 0, "",
				[]string{},
				[]string{},
			}
			s.addSearchResult(sr)
		}
	}
	return nil
}

func notR(c rune) bool {
	return c != 'r'
}

func (s *Searcher) searchTarFileReader(r io.Reader, filepath string) error {
	if s.Settings.Verbose {
		tarName := strings.TrimRightFunc(filepath, notR)
		fmt.Printf("Searching tar file %s\n", tarName)
	}
	tr := tar.NewReader(r)
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		if !strings.HasSuffix(hdr.Name, "/") && s.isSearchFile(hdr.Name) {
			err = s.searchFileReader(tr, fmt.Sprintf("{%s}%s", filepath, hdr.Name))
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (s *Searcher) searchGzipFileReader(r io.Reader, filepath string) error {
	if s.Settings.Verbose {
		fmt.Printf("Searching gzip file %s\n", filepath)
	}
	gr, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	defer func() {
		gr.Close()
	}()
	if strings.HasSuffix(filepath, "tar.gz") || strings.HasSuffix(filepath, "tgz") {
		if err = s.searchTarFileReader(gr, filepath); err != nil {
			return err
		}
	} else {
		if s.isSearchFile(gr.Name) {
			err = s.searchFileReader(gr, fmt.Sprintf("{%s}%s", filepath, gr.Name))
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (s *Searcher) searchBzip2FileReader(r io.Reader, fp string) error {
	if s.Settings.Verbose {
		fmt.Printf("Searching bzip2 file %s\n", fp)
	}
	br := bzip2.NewReader(r)
	if strings.HasSuffix(fp, "tar.bz2") {
		if err := s.searchTarFileReader(br, fp); err != nil {
			return err
		}
	} else {
		containedFileName := strings.TrimSuffix(filepath.Base(fp), ".bz2")
		if s.isSearchFile(containedFileName) {
			err := s.searchFileReader(br, fmt.Sprintf("{%s}%s", fp, containedFileName))
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (s *Searcher) searchZipFileReader(r io.Reader, filepath string) error {
	// zip.OpenReader returns a *zip.ReaderCloser struct type that extends Reader
	zr, err := zip.OpenReader(filepath)
	if err != nil {
		return err
	}
	defer zr.Close()
	// f is a zip.File struct type
	for _, f := range zr.File {
		if f.FileHeader.Flags != 0 && f.FileHeader.Flags != 2 {
			fmt.Printf("%s is an UNKNOWN file type\n", f.Name)
		}
		// f.FileHeader.Flags == 2 seems to mean it's a file (not a dir, etc.)
		if f.FileHeader.Flags == 2 && s.isSearchFile(f.Name) {
			switch s.fileUtil.getFileType(f.Name) {
			case FILETYPE_TEXT:
				if s.Settings.Verbose {
					fmt.Printf("Searching text file {%s}%s\n", filepath, f.Name)
				}
				rc, err := f.Open()
				if err != nil {
					return err
				}
				s.searchTextFileReader(rc, fmt.Sprintf("{%s}%s", filepath, f.Name))
				rc.Close()
			case FILETYPE_BINARY:
				if s.Settings.Verbose {
					fmt.Printf("Searching binary file {%s}%s\n", filepath, f.Name)
				}
				rc, err := f.Open()
				if err != nil {
					return err
				}
				s.searchBinaryFileReader(rc, fmt.Sprintf("{%s}%s", filepath, f.Name))
				rc.Close()
			default:
				fmt.Sprintf("Skipping unknown file type: %s", f.Name)
			}
		}
	}
	return nil
}

func (s *Searcher) searchCompressedFileReader(r io.Reader, filepath string) error {
	ext := s.fileUtil.getExtension(filepath)
	switch ext {
	case "zip", "jar", "war", "ear":
		return s.searchZipFileReader(r, filepath)
	case "gz", "tgz":
		return s.searchGzipFileReader(r, filepath)
	case "bz2":
		return s.searchBzip2FileReader(r, filepath)
	default:
		fmt.Printf("Searching not currently supported for %s files\n", ext)
		return nil
	}
}

func (s *Searcher) searchFileReader(r io.Reader, filepath string) error {
	if s.Settings.Verbose {
		fmt.Printf("Searching file %s\n", filepath)
	}
	switch s.fileUtil.getFileType(filepath) {
	case FILETYPE_TEXT:
		s.searchTextFileReader(r, filepath)
	case FILETYPE_BINARY:
		s.searchBinaryFileReader(r, filepath)
	case FILETYPE_COMPRESSED:
		if s.Settings.SearchCompressed {
			return s.searchCompressedFileReader(r, filepath)
		} else {
			if s.Settings.Verbose {
				fmt.Printf("Skipping compressed file: %s\n", filepath)
			}
			return nil
		}
	//default:
	//	//panic(fmt.Sprintf("Unknown file type: %s", file))
	//	// for now we assume it's a text file, but will probably change this
	//	return s.searchTextFile(file)
	default:
		fmt.Sprintf("Skipping unknown file type: %s", filepath)
	}
	return nil
}

func (s *Searcher) SearchFile(filepath string) error {
	if !s.fileUtil.IsSearchableFile(filepath) {
		if contains(s.Settings.InExtensions, s.fileUtil.getExtension(filepath)) {
			if s.Settings.Debug {
				fmt.Printf("File made searchable by passing in-ext: %s\n", filepath)
			}
		} else {
			if s.Settings.Verbose {
				fmt.Printf("Skipping unsearchable file: %s\n", filepath)
			}
			return nil
		}
	}
	// turn the file into an io.Reader
	file, err := os.Open(filepath)
	if err != nil {
		return err
	}
	defer file.Close()
	return s.searchFileReader(file, filepath)
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
	if s.Settings.PrintResults {
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
	fmt.Printf("Elapsed time for %s: %v", name, elapsed)
}

func (s *Searcher) Search() error {
	if err := s.validSettings(); err != nil {
		return err
	}

	//s.testFileTypeChecking()

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
		fmt.Printf("\nDirectories to be searched (%d):\n", len(s.searchDirs))
		for _, d := range s.searchDirs {
			fmt.Println(d)
		}
	}

	// get search file list
	if s.Settings.DoTiming {
		s.startTimer("setSearchFiles")
	}
	err = s.setSearchFiles()
	if s.Settings.DoTiming {
		s.stopTimer("setSearchFiles")
	}
	if err != nil {
		return err
	}

	if s.Settings.Verbose {
		fmt.Printf("\nFiles to be searched (%d):\n", len(s.searchFiles))
		for _, f := range s.searchFiles {
			fmt.Println(f)
		}
	}

	// search the files
	if s.Settings.Verbose {
		fmt.Println("\nStarting file search...\n")
	}
	if s.Settings.DoTiming {
		s.startTimer("searchFiles")
	}
	for _, f := range s.searchFiles {
		err = s.SearchFile(f)
		if err != nil {
			return err
		}
	}
	if s.Settings.DoTiming {
		s.stopTimer("searchFiles")
	}
	if s.Settings.Verbose {
		fmt.Println("\nFile search complete.\n")
	}

	if s.Settings.PrintResults && s.searchResults.HasSearchResults() {
		fmt.Println()
		s.searchResults.PrintSearchResults()
		fmt.Println()
		patternKeys := []string{}
		for _, p := range s.Settings.SearchPatterns {
			patternKeys = append(patternKeys, p.String())
		}
		s.searchResults.PrintPatternCounts(patternKeys)

		if s.Settings.ListDirs {
			fmt.Println()
			s.searchResults.PrintDirCounts()
		}

		if s.Settings.ListFiles {
			fmt.Println()
			s.searchResults.PrintFileCounts()
		}

		if s.Settings.ListLines {
			fmt.Println()
			s.searchResults.PrintLineCounts()
		}
	}
	return nil
}
