package gosearch

import "testing"

func TestDefaultSearchSettings(t *testing.T) {
	settings := GetDefaultSearchSettings()
	if settings.ArchivesOnly() ||
		settings.Debug() ||
		settings.FirstMatch() ||
		settings.FollowSymlinks() ||
		settings.IncludeHidden() ||
		settings.MultiLineSearch() ||
		settings.PrintDirs() ||
		settings.PrintFiles() ||
		settings.PrintLines() ||
		settings.PrintMatches() ||
		settings.PrintResults() ||
		settings.PrintUsage() ||
		settings.PrintVersion() ||
		!settings.Recursive() ||
		settings.SearchArchives() ||
		settings.UniqueLines() ||
		settings.Verbose() {
		t.Errorf("settings did not match defaults")
	}
}

func TestAddPattern(t *testing.T) {
	settings := GetDefaultSearchSettings()
	settings.AddSearchPattern("Searcher")
	if settings.SearchPatterns().IsEmpty() {
		t.Errorf("SearchPatterns should not be empty")
	}
}

func TestAddExtensions(t *testing.T) {
	settings := GetDefaultSearchSettings()
	settings.AddInExtension("go,hs")
	if len(settings.InExtensions()) != 2 {
		t.Errorf("InExtensions should have two elements")
	}
}

func TestSetArchivesOnly(t *testing.T) {
	settings := GetDefaultSearchSettings()
	settings.SetArchivesOnly(true)
	if !settings.ArchivesOnly() {
		t.Errorf("ArchivesOnly should be true")
	}
	if !settings.SearchArchives() {
		t.Errorf("SearchArchives should be true")
	}
}
