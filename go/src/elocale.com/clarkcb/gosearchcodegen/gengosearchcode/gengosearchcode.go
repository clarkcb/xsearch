package main

import (
	"elocale.com/clarkcb/gosearchcodegen"
	"path/filepath"
)

func main() {
	targetPath := gosearchcodegen.NormalizePath("~/src/git/xsearch/go/src/elocale.com/clarkcb/xsearch")

	// generate the filetypes file
	fileTypesFilePath := filepath.Join(targetPath, "filetypesgen.go")
	gosearchcodegen.GenFileTypesFile(fileTypesFilePath)

	// generate the searchoptions file
	searchOptionsFilePath := filepath.Join(targetPath, "searchoptionsgen.go")
	gosearchcodegen.GenSearchOptionsFile(searchOptionsFilePath)
}
