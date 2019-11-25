package xsearch

import (
	"encoding/json"
	"io/ioutil"
	"strings"
)

type FileType int

const (
	FILETYPE_UNKNOWN FileType = iota
	FILETYPE_ARCHIVE FileType = iota
	FILETYPE_BINARY  FileType = iota
	FILETYPE_CODE    FileType = iota
	FILETYPE_TEXT    FileType = iota
	FILETYPE_XML     FileType = iota
)

type FileTypes struct {
	fileTypeMap map[string]set
}

// used for unmarshalling
type JsonFileType struct {
	Type       string
	Extensions []string
}

type JsonFileTypes struct {
	FileTypes []*JsonFileType
}

func FileTypesFromJson() *FileTypes {
	var fileTypes FileTypes
	fileTypes.fileTypeMap = make(map[string]set)
	data, err := ioutil.ReadFile(FILETYPESPATH)
	if err != nil {
		return &fileTypes
	}
	var jsonFileTypes JsonFileTypes
	if err = json.Unmarshal(data, &jsonFileTypes); err != nil {
		return &fileTypes
	}
	for _, ft := range jsonFileTypes.FileTypes {
		fileTypes.fileTypeMap[ft.Type] = makeSet(ft.Extensions)
	}
	return &fileTypes
}

func (f *FileTypes) getFileType(file string) FileType {
	if f.IsCodeFile(file) {
		return FILETYPE_CODE
	}
	if f.IsXmlFile(file) {
		return FILETYPE_XML
	}
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

func getFileTypeForName(name string) FileType {
	if strings.ToUpper(name) == "TEXT" {
		return FILETYPE_TEXT
	}
	if strings.ToUpper(name) == "BINARY" {
		return FILETYPE_BINARY
	}
	if strings.ToUpper(name) == "CODE" {
		return FILETYPE_CODE
	}
	if strings.ToUpper(name) == "ARCHIVE" {
		return FILETYPE_ARCHIVE
	}
	if strings.ToUpper(name) == "XML" {
		return FILETYPE_XML
	}
	return FILETYPE_UNKNOWN
}

func getNameForFileType(fileType FileType) string {
	if fileType == FILETYPE_TEXT {
		return "TEXT"
	}
	if fileType == FILETYPE_BINARY {
		return "BINARY"
	}
	if fileType == FILETYPE_CODE {
		return "CODE"
	}
	if fileType == FILETYPE_ARCHIVE {
		return "ARCHIVE"
	}
	if fileType == FILETYPE_XML {
		return "XML"
	}
	return "UNKNOWN"
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

func (f *FileTypes) IsCodeFile(file string) bool {
	return f.isFileType("code", file)
}

func (f *FileTypes) IsTextFile(file string) bool {
	textTypes := [...]string{"code", "text", "xml"}
	for _, t := range textTypes {
		if f.isFileType(t, file) {
			return true
		}
	}
	return false
}

func (f *FileTypes) IsXmlFile(file string) bool {
	return f.isFileType("xml", file)
}

func (f *FileTypes) IsSearchableFile(file string) bool {
	return f.IsCodeFile(file) || f.IsXmlFile(file) || f.IsTextFile(file) || f.IsBinaryFile(file) ||
		f.IsArchiveFile(file)
}

func (f *FileTypes) IsSearchableItem(si *SearchItem) bool {
	return si.fileType == FILETYPE_CODE || si.fileType == FILETYPE_XML || si.fileType == FILETYPE_TEXT ||
		si.fileType == FILETYPE_BINARY || si.fileType == FILETYPE_ARCHIVE
}

func (f *FileTypes) IsUnknownFile(file string) bool {
	return f.getFileType(file) == FILETYPE_UNKNOWN
}
