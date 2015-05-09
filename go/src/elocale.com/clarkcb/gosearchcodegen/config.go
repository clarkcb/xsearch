package gosearchcodegen

import (
	"path/filepath"
)

var XSEARCHSRCPATH string
var FILETYPESXMLPATH string
var SEARCHOPTIONSXMLPATH string

func SetConfigValues() {
	home := getHome()
	xsearchProjPath := filepath.Join(home, "src", "xsearch")
	goSrcPath := filepath.Join(xsearchProjPath, "go", "src")
	XSEARCHSRCPATH = filepath.Join(goSrcPath, "elocale.com", "clarkcb", "xsearch")
	sharedPath := filepath.Join(xsearchProjPath, "shared")
	FILETYPESXMLPATH = filepath.Join(sharedPath, "filetypes.xml")
	SEARCHOPTIONSXMLPATH = filepath.Join(sharedPath, "searchoptions.xml")
}
