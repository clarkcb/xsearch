package gosearch

import (
	"os"
	"path/filepath"
)

type SearchConfig struct {
	XSEARCHPATH       string
	SHAREDPATH        string
	SEARCHOPTIONSPATH string
	VERSION           string
}

func NewSearchConfig() *SearchConfig {
	xSearchPath := os.Getenv("XSEARCH_PATH")
	if xSearchPath == "" {
		xSearchPath = filepath.Join(os.Getenv("HOME"), "src/xsearch")
	}
	sharedPath := filepath.Join(xSearchPath, "shared")

	return &SearchConfig{
		xSearchPath,
		sharedPath,
		filepath.Join(sharedPath, "searchoptions.json"),
		"0.1.0",
	}
}
