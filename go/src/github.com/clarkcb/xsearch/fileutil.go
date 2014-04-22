package xsearch

import (
	"encoding/xml"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

type FileUtil struct {
	fileTypeMap map[string]set
}

func NewFileUtil() *FileUtil {
	return &FileUtil{
		getFileTypeMap(),
	}
}

func (f *FileUtil) getExtension(file string) string {
	ext := filepath.Ext(file)
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

type FileType int

const (
	FILETYPE_UNKNOWN    FileType = iota
	FILETYPE_BINARY     FileType = iota
	FILETYPE_COMPRESSED FileType = iota
	FILETYPE_TEXT       FileType = iota
)

func (f *FileUtil) getFileType(file string) FileType {
	if f.IsBinaryFile(file) {
		return FILETYPE_BINARY
	} else if f.IsCompressedFile(file) {
		return FILETYPE_COMPRESSED
	} else if f.IsTextFile(file) {
		return FILETYPE_TEXT
	} else {
		return FILETYPE_UNKNOWN
	}
}

func (f *FileUtil) isFileType(filetype string, file string) bool {
	return f.fileTypeMap[filetype][f.getExtension(file)]
}

// going to assume file is binary if it has no extension
func (f *FileUtil) IsBinaryFile(file string) bool {
	return f.isFileType("binary", file) || f.getExtension(file) == ""
}

func (f *FileUtil) IsCompressedFile(file string) bool {
	return f.isFileType("compressed", file)
}

func (f *FileUtil) IsTextFile(file string) bool {
	textTypes := []string{"text", "code", "xml"}
	for _, t := range textTypes {
		if f.isFileType(t, file) {
			return true
		}
	}
	return false
}

func (f *FileUtil) IsSearchableFile(file string) bool {
	return f.IsTextFile(file) || f.IsBinaryFile(file) || f.IsCompressedFile(file)
}

type XmlFileTypes struct {
	XmlFileTypes []XmlFileType `xml:"filetype"`
}

type XmlFileType struct {
	Name       string        `xml:"name,attr"`
	Extensions XmlExtensions `xml:"extensions"`
}

type XmlExtensions struct {
	Extensions string `xml:",chardata"`
}

const fileTypesXmlPath = "/Users/cary/src/git/xsearch/shared/filetypes.xml"

func getFiletypesFromXml() *XmlFileTypes {
	file, err := os.Open(fileTypesXmlPath)

	if err != nil {
		panic(err.Error())
	}

	defer func() {
		err := file.Close()
		if err != nil {
			panic(err.Error())
		}
	}()

	xmlFileTypes := &XmlFileTypes{}
	decoder := xml.NewDecoder(file)

	if err := decoder.Decode(xmlFileTypes); err != nil {
		panic(err.Error())
	}
	return xmlFileTypes
}

func getFileTypeMap() map[string]set {
	xmlFileTypes := getFiletypesFromXml()
	fileTypeMap := make(map[string]set)
	r := regexp.MustCompile("\\s+")
	for _, x := range xmlFileTypes.XmlFileTypes {
		fileTypeMap[x.Name] = makeSet(r.Split(x.Extensions.Extensions, -1))
	}
	return fileTypeMap
}
