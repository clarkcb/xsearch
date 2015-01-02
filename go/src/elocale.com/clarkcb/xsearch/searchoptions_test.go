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

	if len(settings.InExtensions) != 1 {
		t.Errorf("len(settings.InExtensions) = %i, expected 1",
			len(settings.InExtensions))
	}
	expectedExt := "go"
	if *settings.InExtensions[0] != expectedExt {
		t.Errorf("settings.InExtensions[0] (\"%s\") != \"%s\"",
			*settings.InExtensions[0], expectedExt)
	}
}
