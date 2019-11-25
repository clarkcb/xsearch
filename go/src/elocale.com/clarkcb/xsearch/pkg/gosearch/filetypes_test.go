package gosearch

import "testing"

func TestGetFileType(t *testing.T) {
	expected := map[string]FileType{
		"hello.txt":             FILETYPE_TEXT,
		"filetypes.go":          FILETYPE_CODE,
		"markup.xml":            FILETYPE_XML,
		"lib.a":                 FILETYPE_BINARY,
		"noext":                 FILETYPE_BINARY,
		"archive.tar.gz":        FILETYPE_ARCHIVE,
		"nonsense.zippitydooda": FILETYPE_UNKNOWN,
	}

	fileTypes := FileTypesFromJson()

	for k, v := range expected {
		if ft := fileTypes.getFileType(k); ft != v {
			t.Errorf("getFileType(\"%s\")=\"%v\", expected=\"%v\"", k, ft, v)
		}
	}
}

func TestIsSearchableFile(t *testing.T) {
	expected := map[string]bool{
		"hello.txt":             true,
		"filetypes.go":          true,
		"markup.xml":            true,
		"lib.a":                 true,
		"noext":                 true,
		"archive.tar.gz":        true,
		"nonsense.zippitydooda": false,
	}

	fileTypes := FileTypesFromJson()

	for k, v := range expected {
		if is := fileTypes.IsSearchableFile(k); is != v {
			t.Errorf("getFileType(\"%s\")=\"%t\", expected=\"%t\"", k, is, v)
		}
	}
}
