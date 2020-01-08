package gosearch

import (
	"encoding/json"
	"io/ioutil"
	"strings"
)

type FileType int

const (
	FiletypeUnknown FileType = iota
	FiletypeArchive FileType = iota
	FiletypeBinary  FileType = iota
	FiletypeCode    FileType = iota
	FiletypeText    FileType = iota
	FiletypeXml     FileType = iota
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
		return FiletypeCode
	}
	if f.IsXmlFile(file) {
		return FiletypeXml
	}
	if f.IsTextFile(file) {
		return FiletypeText
	}
	if f.IsBinaryFile(file) {
		return FiletypeBinary
	}
	if f.IsArchiveFile(file) {
		return FiletypeArchive
	}
	return FiletypeUnknown
}

func getFileTypeForName(name string) FileType {
	if strings.ToUpper(name) == "TEXT" {
		return FiletypeText
	}
	if strings.ToUpper(name) == "BINARY" {
		return FiletypeBinary
	}
	if strings.ToUpper(name) == "CODE" {
		return FiletypeCode
	}
	if strings.ToUpper(name) == "ARCHIVE" {
		return FiletypeArchive
	}
	if strings.ToUpper(name) == "XML" {
		return FiletypeXml
	}
	return FiletypeUnknown
}

func getNameForFileType(fileType FileType) string {
	if fileType == FiletypeText {
		return "TEXT"
	}
	if fileType == FiletypeBinary {
		return "BINARY"
	}
	if fileType == FiletypeCode {
		return "CODE"
	}
	if fileType == FiletypeArchive {
		return "ARCHIVE"
	}
	if fileType == FiletypeXml {
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
	return si.fileType == FiletypeCode || si.fileType == FiletypeXml || si.fileType == FiletypeText ||
		si.fileType == FiletypeBinary || si.fileType == FiletypeArchive
}

func (f *FileTypes) IsUnknownFile(file string) bool {
	return f.getFileType(file) == FiletypeUnknown
}
