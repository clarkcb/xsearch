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
