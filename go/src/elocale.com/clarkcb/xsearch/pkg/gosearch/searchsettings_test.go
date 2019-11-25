package gosearch

import "testing"

func TestDefaultSearchSettings(t *testing.T) {
	settings := GetDefaultSearchSettings()
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

func TestAddPattern(t *testing.T) {
	settings := GetDefaultSearchSettings()
	settings.AddSearchPattern("Searcher")
	if settings.SearchPatterns.IsEmpty() {
		t.Errorf("SearchPatterns should not be empty")
	}
}

func TestAddExtensions(t *testing.T) {
	settings := GetDefaultSearchSettings()
	settings.AddInExtension("go,hs")
	if len(settings.InExtensions) != 2 {
		t.Errorf("InExtensions should have two elements")
	}
}
