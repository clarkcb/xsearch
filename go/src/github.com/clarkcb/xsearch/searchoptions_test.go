package xsearch

import "testing"

func TestSearchSettingsFromArgs(t *testing.T) {
	searchOptions := NewSearchOptions()

	args := []string{
		"-x", "go", "-s", "Searcher", ".",
	}

	settings, err := searchOptions.SearchSettingsFromArgs(args)
	if err != nil {
		t.Errorf("SearchSettingsFromArgs: err: %v", err)
	}

	if settings.StartPath != "." {
		t.Errorf("settings.StartPath (%s) != \".\"", settings.StartPath)
	}

	if len(settings.InExtensions) != 1 || settings.InExtensions[0] != "go" {
		t.Errorf("settings.InExtensions (%v) != []string{\"go\"}",
			settings.InExtensions)
	}
}
