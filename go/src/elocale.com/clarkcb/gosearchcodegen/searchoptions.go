package gosearchcodegen

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"
)

type SearchOption struct {
	Short string
	Long  string
	Desc  string
}

type SearchOptions struct {
	SearchOptions []SearchOption
}

func NewSearchOptions() *SearchOptions {
	searchOptions, err := searchOptionsFromXml()
	if err != nil {
		panic(err.Error())
	}
	return searchOptions
}

func escapeQuotes(s string) string {
	return strings.Replace(s, "\"", "\\\"", -1)
}

func GetSearchOptionsString() string {
	searchOptions := NewSearchOptions()
	var buffer bytes.Buffer
	depth := 0
	buffer.WriteString("package xsearch\n\n")
	buffer.WriteString("func GetSearchOptions() *SearchOptions {\n")
	depth++
	buffer.WriteString(fmt.Sprintf("%sreturn &SearchOptions{\n", strings.Repeat("\t", depth)))
	depth++
	buffer.WriteString(fmt.Sprintf("%s[]*SearchOption{\n", strings.Repeat("\t", depth)))
	depth++
	for _, so := range searchOptions.SearchOptions {
		buffer.WriteString(fmt.Sprintf("%s&SearchOption{\"%s\", \"%s\", \"%s\"},\n",
			strings.Repeat("\t", depth), so.Short, so.Long, escapeQuotes(so.Desc)))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	buffer.WriteString(fmt.Sprintf("%s}\n}\n", strings.Repeat("\t", depth)))
	return buffer.String()
}

func GenSearchOptionsFile(path string) {
	searchOptionsString := GetSearchOptionsString()
	ioutil.WriteFile(path, []byte(searchOptionsString), 0644)
}

type XmlSearchOptions struct {
	XmlSearchOptions []XmlSearchOption `xml:"searchoption"`
}

type XmlSearchOption struct {
	Short string `xml:"short,attr"`
	Long  string `xml:"long,attr"`
	Desc  string `xml:",chardata"`
}

const searchOptionsXmlPath = "/Users/cary/src/git/xsearch/shared/searchoptions.xml"

func searchOptionsFromXml() (*SearchOptions, error) {
	var searchOptions []SearchOption
	xmlSearchOptions := &XmlSearchOptions{}

	if err := loadXmlFile(searchOptionsXmlPath, xmlSearchOptions); err != nil {
		return nil, err
	}

	for _, x := range xmlSearchOptions.XmlSearchOptions {
		searchOption := &SearchOption{x.Short, x.Long, strings.TrimSpace(x.Desc)}
		searchOptions = append(searchOptions, *searchOption)
	}
	return &SearchOptions{searchOptions}, nil
}
