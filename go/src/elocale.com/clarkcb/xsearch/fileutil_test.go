package xsearch

import "testing"

func TestGetExtension(t *testing.T) {
	expected := map[string]string{
		"hello.txt":      "txt",
		"lib.a":          "a",
		"noext":          "",
		"archive.tar.gz": "gz",
	}

	for k, v := range expected {
		if ext := getExtension(k); ext != v {
			t.Errorf("getExtension(\"%s\")=\"%s\", expected=\"%s\"", k, ext, v)
		}
	}
}

func TestGetFileType(t *testing.T) {
	expected := map[string]FileType{
		"hello.txt":             FILETYPE_TEXT,
		"lib.a":                 FILETYPE_BINARY,
		"noext":                 FILETYPE_BINARY,
		"archive.tar.gz":        FILETYPE_ARCHIVE,
		"nonsense.zippitydooda": FILETYPE_UNKNOWN,
	}

	fileTypes := GetFileTypes()

	for k, v := range expected {
		if ft := fileTypes.getFileType(k); ft != v {
			t.Errorf("getFileType(\"%s\")=\"%v\", expected=\"%v\"", k, ft, v)
		}
	}
}

func TestIsSearchableFile(t *testing.T) {
	expected := map[string]bool{
		"hello.txt":             true,
		"lib.a":                 true,
		"noext":                 true,
		"archive.tar.gz":        true,
		"nonsense.zippitydooda": false,
	}

	fileTypes := GetFileTypes()

	for k, v := range expected {
		if is := fileTypes.IsSearchableFile(k); is != v {
			t.Errorf("getFileType(\"%s\")=\"%t\", expected=\"%t\"", k, is, v)
		}
	}
}
