package gosearchcodegen

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"regexp"
	"strings"
)

type FileType struct {
	Name       string
	Extensions set
}

type FileTypes struct {
	FileTypes []FileType
}

func NewFileTypes() *FileTypes {
	fileTypes, err := getFileTypesFromXml()
	if err != nil {
		panic(err.Error())
	}
	return fileTypes
}

func GetFileTypesString() string {
	fileTypes := NewFileTypes()
	var buffer bytes.Buffer
	depth := 0
	buffer.WriteString("package xsearch\n\n")
	buffer.WriteString("func GetFileTypes() *FileTypes {\n")
	depth++
	buffer.WriteString(fmt.Sprintf("%sreturn &FileTypes{\n", strings.Repeat("\t", depth)))
	depth++
	buffer.WriteString(fmt.Sprintf("%smap[string]set{\n", strings.Repeat("\t", depth)))
	depth++
	for _, ft := range fileTypes.FileTypes {
		buffer.WriteString(fmt.Sprintf("%s\"%s\": makeSet([]string{\"%s\"}),\n",
			strings.Repeat("\t", depth), ft.Name, strings.Join(getSortedValues(ft.Extensions), "\", \"")))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	buffer.WriteString(fmt.Sprintf("%s}\n}\n", strings.Repeat("\t", depth)))
	return buffer.String()
}

func GenFileTypesFile(path string) {
	fileTypesString := GetFileTypesString()
	ioutil.WriteFile(path, []byte(fileTypesString), 0644)
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

func getFileTypesFromXml() (*FileTypes, error) {
	xmlFileTypes := &XmlFileTypes{}
	if err := loadXmlFile(FILETYPESXMLPATH, xmlFileTypes); err != nil {
		return nil, err
	}
	var fileTypes []FileType
	r := regexp.MustCompile("\\s+")
	for _, x := range xmlFileTypes.XmlFileTypes {
		fileType := &FileType{x.Name, makeSet(r.Split(x.Extensions.Extensions, -1))}
		fileTypes = append(fileTypes, *fileType)
	}
	return &FileTypes{fileTypes}, nil
}
