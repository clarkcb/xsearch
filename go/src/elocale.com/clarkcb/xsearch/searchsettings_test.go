package xsearch

import "testing"

func TestDefaultSearchSettings(t *testing.T) {
	settings := GetDefaultSearchSettings()
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
