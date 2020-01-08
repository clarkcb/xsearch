package gosearch

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

func expandPath(filePath string) string {
	if strings.HasPrefix(filePath, "~") {
		home := getHome()
		return home + strings.TrimPrefix(filePath, "~")
	}
	return filePath
}

func getExtension(file string) string {
	ext := filepath.Ext(filepath.Base(file))
	return strings.ToLower(strings.TrimLeft(ext, "."))
}

func getHome() string {
	home := ""
	homeName := "HOME"
	if runtime.GOOS == "windows" {
		homeName = "USERPROFILE"
	}
	env := os.Environ()
	for _, x := range env {
		if strings.HasPrefix(x, homeName+"=") {
			home = strings.TrimPrefix(x, homeName+"=")
			break
		}
	}
	return home
}

func getPathSeparator() string {
	if runtime.GOOS == "windows" {
		return "\\"
	}
	return "/"
}

func isDotDir(file string) bool {
	dotDirs := []string{".", ".."}
	return containsV(dotDirs, file)
}

func isHidden(file string) bool {
	for _, d := range strings.Split(file, getPathSeparator()) {
		if len(d) > 1 && strings.HasPrefix(d, ".") && !isDotDir(d) {
			return true
		}
	}
	return false
}

func normalizePath(path string) string {
	return strings.TrimRight(path, "/\\")
}

func relativePath(path string, startPath string) string {
	homePath := getHome()
	log(fmt.Sprintf("homePath:%s", homePath))
	relativePath := path
	if startPath == "." && strings.HasPrefix(path, homePath) {
		log("path starts with homePath")
		relativePath = "." + strings.TrimPrefix(path, homePath)
	}
	return relativePath
}
