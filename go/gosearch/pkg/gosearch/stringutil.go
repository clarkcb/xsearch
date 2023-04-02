package gosearch

import (
	"bytes"
	"fmt"
	"strings"
)

func stringListToString(list []string) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	var elems []string
	for _, l := range list {
		elems = append(elems, fmt.Sprintf("\"%s\"", l))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
	return buffer.String()
}

// func fileTypeListToString(list []gofind.FileType) string {
func fileTypeListToString(list []FileType) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	var elems []string
	for _, ft := range list {
		//elems = append(elems, fmt.Sprintf("\"%s\"", gofind.GetNameForFileType(ft)))
		elems = append(elems, fmt.Sprintf("\"%s\"", getNameForFileType(ft)))
	}
	buffer.WriteString(strings.Join(elems, ","))
	buffer.WriteString("]")
	return buffer.String()
}

func searchPatternsToString(sp *SearchPatterns) string {
	var buffer bytes.Buffer
	buffer.WriteString("[")
	for i, r := range sp.patterns {
		if i > 0 {
			buffer.WriteString(",")
		}
		buffer.WriteString(fmt.Sprintf("\"%s\"", r.String()))
	}
	buffer.WriteString("]")
	return buffer.String()
}
