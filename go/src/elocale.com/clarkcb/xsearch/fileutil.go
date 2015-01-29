package xsearch

import (
	"os"
	"path"
	"path/filepath"
	"strings"
)

func getExtension(file string) string {
	ext := filepath.Ext(path.Base(file))
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

func isDotDir(file string) bool {
	dotDirs := []string{".", ".."}
	if containsV(dotDirs, file) {
		return true
	}
	return false
}

func isHidden(file string) bool {
	f := path.Base(file)
	if len(f) > 1 && strings.HasPrefix(f, ".") && !isDotDir(f) {
		return true
	}
	return false
}

func normalizePath(path string) string {
	return strings.TrimRight(path, "/")
}
