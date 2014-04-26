package main

import (
	"github.com/clarkcb/gosearchcodegen"
	"path/filepath"
)

func main() {
	targetPath := "/Users/cary/src/git/xsearch/go/src/github.com/clarkcb/xsearch"
	
	// generate the searchoptions file
	searchOptionsFilePath := filepath.Join(targetPath, "searchoptionsgen.go")
	gosearchcodegen.GenSearchOptionsFile(searchOptionsFilePath)
	
	// generate the filetypes file
	fileTypesFilePath := filepath.Join(targetPath, "filetypesgen.go")
	gosearchcodegen.GenFileTypesFile(fileTypesFilePath)
}