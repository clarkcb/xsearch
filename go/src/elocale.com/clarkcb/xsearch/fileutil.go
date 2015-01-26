package xsearch

import (
	"os"
	"path/filepath"
	"strings"
)

func getExtension(file string) string {
	ext := filepath.Ext(file)
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

func getHome() string {
	home := ""
	env := os.Environ()
	for _, x := range env {
		if strings.HasPrefix(x, "HOME=") {
			home = strings.TrimPrefix(x, "HOME=")
			break
		}
	}
	return home
}

func expandPath(filePath string) string {
	if strings.HasPrefix(filePath, "~") {
		home := getHome()
		return home + strings.TrimPrefix(filePath, "~")
	}
	return filePath
}

func normalizePath(path string) string {
	return strings.TrimRight(path, "/")
}
