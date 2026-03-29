package gosearch

import (
	"os"
	"path/filepath"
)

type SearchConfig struct {
	XSEARCHPATH               string
	SHAREDPATH                string
	SEARCHOPTIONSPATH         string
	DEFAULTSEARCHSETTINGSPATH string
	VERSION                   string
}

func NewSearchConfig() *SearchConfig {
	home := os.Getenv("HOME")
	xSearchPath := os.Getenv("XSEARCH_PATH")
	if xSearchPath == "" {
		xSearchPath = filepath.Join(home, "src/xsearch")
	}
	sharedPath := filepath.Join(xSearchPath, "shared")
	defaultSearchSettingsPath := filepath.Join(home, ".config/xsearch/settings.json")

	return &SearchConfig{
		xSearchPath,
		sharedPath,
		filepath.Join(sharedPath, "searchoptions.json"),
		defaultSearchSettingsPath,
		"0.1.0",
	}
}
