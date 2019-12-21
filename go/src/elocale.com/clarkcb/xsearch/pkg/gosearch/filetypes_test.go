package gosearch

import "testing"

func TestGetFileType(t *testing.T) {
	expected := map[string]FileType{
		"hello.txt":             FiletypeText,
		"filetypes.go":          FiletypeCode,
		"markup.xml":            FiletypeXml,
		"lib.a":                 FiletypeBinary,
		"noext":                 FiletypeBinary,
		"archive.tar.gz":        FiletypeArchive,
		"nonsense.zippitydooda": FiletypeUnknown,
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
