package main

import (
	"elocale.com/clarkcb/gosearchcodegen"
	"path/filepath"
)

func main() {
	gosearchcodegen.SetConfigValues()

	// generate the filetypes file
	fileTypesFilePath := filepath.Join(gosearchcodegen.XSEARCHSRCPATH, "filetypesgen.go")
	gosearchcodegen.GenFileTypesFile(fileTypesFilePath)

	// generate the searchoptions file
	searchOptionsFilePath := filepath.Join(gosearchcodegen.XSEARCHSRCPATH, "searchoptionsgen.go")
	gosearchcodegen.GenSearchOptionsFile(searchOptionsFilePath)
}
