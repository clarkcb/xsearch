package gosearch

import (
	"gofind/pkg/gofind"
	"os"
	"path/filepath"
)

type SearchConfig struct {
	FindConfig                *gofind.FindConfig
	XSearchPath               string
	SearchOptionsPath         string
	DefaultSearchSettingsPath string
	Version                   string
}

func NewSearchConfig() *SearchConfig {
	home := os.Getenv("HOME")
	defaultXSearchConfigDir := filepath.Join(home, ".config", "xsearch")
	xSearchConfigDir := os.Getenv("XSEARCH_CONFIG_DIR")
	if xSearchConfigDir == "" {
		xSearchConfigDir = defaultXSearchConfigDir
	}
	defaultXSearchPath := filepath.Join(home, "src", "xsearch")
	xSearchPath := os.Getenv("XSEARCH_PATH")
	if xSearchPath == "" {
		xSearchPath = defaultXSearchPath
	}
	sharedPath := filepath.Join(xSearchPath, "shared")
	defaultSearchSettingsPath := filepath.Join(xSearchConfigDir, "settings.json")

	return &SearchConfig{
		gofind.NewFindConfig(),
		xSearchPath,
		filepath.Join(sharedPath, "searchoptions.json"),
		defaultSearchSettingsPath,
		"0.1.0",
	}
}
