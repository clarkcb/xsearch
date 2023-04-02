package gosearch

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

	if len(settings.Paths) != 1 {
		t.Errorf("settings.Paths is empty")
	}

	if settings.Paths[0] != "." {
		t.Errorf("settings.Paths[0] != \".\"")
	}

	if len(settings.InExtensions) != 1 {
		t.Errorf("len(settings.InExtensions) = %d, expected 1",
			len(settings.InExtensions))
	}
	expectedExt := "go"
	if settings.InExtensions[0] != expectedExt {
		t.Errorf("settings.InExtensions[0] (\"%s\") != \"%s\"",
			settings.InExtensions[0], expectedExt)
	}
}

func TestSearchSettingsFromJson(t *testing.T) {
	searchOptions := NewSearchOptions()

	jsonSettings := []byte(`{
  "path": "~/src/xsearch/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "searchpattern": "Searcher",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}`)

	settings := GetDefaultSearchSettings()
	var err error
	err = searchOptions.SettingsFromJson(jsonSettings, settings)
	if err != nil {
		t.Errorf("TestSearchSettingsFromJson: err: %v", err)
	}

	if len(settings.Paths) != 1 {
		t.Errorf("settings.Paths is empty")
	}

	if settings.Paths[0] != "~/src/xsearch/" {
		t.Errorf("settings.Paths[0] != \"~/src/xsearch/\"")
	}

	if len(settings.InExtensions) != 2 {
		t.Errorf("len(settings.InExtensions) = %d, expected 2",
			len(settings.InExtensions))
	}
	expectedInExts := []string{"js", "ts"}
	for i, _ := range expectedInExts {
		if settings.InExtensions[i] != expectedInExts[i] {
			t.Errorf("settings.InExtensions[%d] (\"%s\") != \"%s\"",
				i, settings.InExtensions[i], expectedInExts[i])
		}
	}

	if len(settings.OutDirPatterns.patterns) != 1 {
		t.Errorf("len(settings.OutDirPatterns.patterns) = %d, expected 1",
			len(settings.OutDirPatterns.patterns))
	}
	if settings.OutDirPatterns.patterns[0].String() != "node_module" {
		t.Errorf("settings.OutDirPatterns.patterns[0].String() (\"%s\") != \"node_module\"",
			settings.OutDirPatterns.patterns[0].String())
	}

	if len(settings.OutFilePatterns.patterns) != 1 {
		t.Errorf("len(settings.OutFilePatterns.patterns) = %d, expected 1",
			len(settings.OutFilePatterns.patterns))
	}
	if settings.OutFilePatterns.patterns[0].String() != "temp" {
		t.Errorf("settings.OutFilePatterns.patterns[0].String() (\"%s\") != \"temp\"",
			settings.OutFilePatterns.patterns[0].String())
	}

	if len(settings.SearchPatterns.patterns) != 1 {
		t.Errorf("len(settings.SearchPatterns.patterns) = %d, expected 1",
			len(settings.SearchPatterns.patterns))
	}
	if settings.SearchPatterns.patterns[0].String() != "Searcher" {
		t.Errorf("settings.SearchPatterns.patterns[0].String() (\"%s\") != \"Searcher\"",
			settings.SearchPatterns.patterns[0].String())
	}

	if settings.LinesBefore != 2 {
		t.Errorf("settings.LinesBefore (%d) != 2", settings.LinesBefore)
	}

	if settings.LinesAfter != 2 {
		t.Errorf("settings.LinesAfter (%d) != 2", settings.LinesAfter)
	}

	if !settings.Debug {
		t.Errorf("settings.Debug (%t) != true", settings.Debug)
	}

	if !settings.FirstMatch {
		t.Errorf("settings.FirstMatch (%t) != true", settings.FirstMatch)
	}

	if settings.ExcludeHidden {
		t.Errorf("settings.ExcludeHidden (%t) != false", settings.ExcludeHidden)
	}
}
