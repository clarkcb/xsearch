package xsearch

import "testing"

func TestSearchSettingsFromNoArgs(t *testing.T) {
	searchOptions := NewSearchOptions()

	args := []string{}

	settings, err := searchOptions.SearchSettingsFromArgs(args)
	if err != nil {
		t.Errorf("SearchSettingsFromArgs: err: %v", err)
	}

	if settings.ArchivesOnly ||
	   settings.Debug ||
	   settings.DoTiming ||
	   !settings.ExcludeHidden ||
	   settings.FirstMatch ||
	   settings.ListDirs ||
	   settings.ListFiles ||
	   settings.ListLines ||
	   settings.MultiLineSearch ||
	   !settings.PrintResults ||
	   settings.PrintUsage ||
	   settings.PrintVersion ||
	   !settings.Recursive ||
	   settings.SearchArchives ||
	   settings.UniqueLines ||
	   settings.Verbose {
		t.Errorf("settings did not match defaults")
	}
}

func TestSearchSettingsFromValidArgs(t *testing.T) {
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
