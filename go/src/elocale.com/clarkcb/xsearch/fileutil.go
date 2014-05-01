package xsearch

import (
	"path/filepath"
	"strings"
)

type FileType int

const (
	FILETYPE_UNKNOWN FileType = iota
	FILETYPE_ARCHIVE FileType = iota
	FILETYPE_BINARY  FileType = iota
	FILETYPE_TEXT    FileType = iota
)

type FileTypes struct {
	fileTypeMap map[string]set
}

func getExtension(file string) string {
	ext := filepath.Ext(file)
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

func (f *FileTypes) getFileType(file string) FileType {
	if f.IsTextFile(file) {
		return FILETYPE_TEXT
	}
	if f.IsBinaryFile(file) {
		return FILETYPE_BINARY
	}
	if f.IsArchiveFile(file) {
		return FILETYPE_ARCHIVE
	}
	return FILETYPE_UNKNOWN
}

func (f *FileTypes) isFileType(filetype string, file string) bool {
	return f.fileTypeMap[filetype][getExtension(file)]
}

func (f *FileTypes) IsArchiveFile(file string) bool {
	return f.isFileType("archive", file)
}

// going to assume file is binary if it has no extension (for now)
func (f *FileTypes) IsBinaryFile(file string) bool {
	return f.isFileType("binary", file) || getExtension(file) == ""
}

func (f *FileTypes) IsTextFile(file string) bool {
	textTypes := []string{"code", "text", "xml"}
	for _, t := range textTypes {
		if f.isFileType(t, file) {
			return true
		}
	}
	return false
}

func (f *FileTypes) IsSearchableFile(file string) bool {
	return f.IsTextFile(file) || f.IsBinaryFile(file) || f.IsArchiveFile(file)
}
