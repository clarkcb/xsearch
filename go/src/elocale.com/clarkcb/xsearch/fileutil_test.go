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

func TestIsDotDir(t *testing.T) {
	expected := map[string]bool{
		".":      true,
		"..":     true,
		"lib.a":  false,
		"noext":  false,
	}

	for k, v := range expected {
		if d := isDotDir(k); d != v {
			t.Errorf("isDotDir(\"%s\")=%v, expected=%v", k, d, v)
		}
	}
}

func TestIsHidden(t *testing.T) {
	expected := map[string]bool{
		".":          false,
		"..":         false,
		"lib.a":      false,
		"noext":      false,
		".git":       true,
		".gitignore": true,
	}

	for k, v := range expected {
		if h := isHidden(k); h != v {
			t.Errorf("isHidden(\"%s\")=%v, expected=%v", k, h, v)
		}
	}
}

func TestNormalizePath(t *testing.T) {
	expected := map[string]string{
		".":          ".",
		"./":         ".",
		"..":         "..",
		"../":        "..",
		"path":       "path",
		"path/":      "path",
		"long/path":  "long/path",
		"long/path/": "long/path",
	}

	for k, v := range expected {
		if p := normalizePath(k); p != v {
			t.Errorf("normalizePath(\"%s\")=\"%s\", expected=\"%s\"", k, p, v)
		}
	}
}
