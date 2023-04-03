package gosearch

import (
	"fmt"
	"path/filepath"
	"testing"
)

func Test_SearchItem_AbsPath(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "/Users/cary/src/xsearch/go/gosearch/pkg/gosearch/searchfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	searchItem := NewSearchItem(dir, file, fileType)
	if searchItem.String() != path {
		t.Errorf(fmt.Sprintf("searchItem.String() (%s) != path (%s)", searchItem.String(), path))
	}
}

func Test_SearchItem_TildePath(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "~/src/xsearch/go/gosearch/pkg/gosearch/searchfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	searchItem := NewSearchItem(dir, file, fileType)
	if searchItem.String() != path {
		t.Errorf(fmt.Sprintf("searchItem.String() (%s) != path (%s)", searchItem.String(), path))
	}
}

func Test_SearchItem_RelPath1(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "./searchfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	searchItem := NewSearchItem(dir, file, fileType)
	searchItemString := searchItem.String()
	if searchItemString != path {
		t.Errorf(fmt.Sprintf("searchItem.String() (%s) != path (%s)", searchItem.String(), path))
	}
}

func Test_SearchItem_RelPath2(t *testing.T) {
	fileTypes := FileTypesFromJson()
	path := "./searchfiles.go"
	dir, file := filepath.Split(path)
	fileType := fileTypes.getFileType(file)
	searchItem := NewSearchItem(dir, file, fileType)
	if searchItem.String() != path {
		t.Errorf(fmt.Sprintf("searchItem.String() (%s) != path (%s)", searchItem.String(), path))
	}
}
