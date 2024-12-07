package gosearch

import (
	"bytes"
	"encoding/json"
	"fmt"
	"gofind/pkg/gofind"
	"os"
	"reflect"
	"strconv"
	"strings"
)

type SearchOption struct {
	Short string
	Long  string
	Desc  string
}

type SearchOptions struct {
	SearchOptions []*SearchOption
}

func SearchOptionsFromJson() (*SearchOptions, error) {
	config := NewSearchConfig()
	data, err := os.ReadFile(config.SEARCHOPTIONSPATH)
	if err != nil {
		return &SearchOptions{}, err
	}
	var searchOptions SearchOptions
	if err = json.Unmarshal(data, &searchOptions); err != nil {
		return &SearchOptions{}, err
	}

	// TEMPORARY
	//searchOptions.generateCodeFile("/Users/cary/src/xsearch/go/gosearch/pkg/gosearch/searchoptionsgen.go")

	return &searchOptions, nil
}

func NewSearchOptions() *SearchOptions {
	searchOptions, err := SearchOptionsFromJson()
	if err != nil {
		// do something
	}
	return searchOptions
}

func (so *SearchOptions) SettingsFromFile(filePath string, settings *SearchSettings) error {
	if data, err := os.ReadFile(filePath); err != nil {
		return err
	} else {
		return so.SettingsFromJson(data, settings)
	}
}

func (so *SearchOptions) SettingsFromJson(data []byte, settings *SearchSettings) error {
	boolActionMap := so.getBoolActionMap()
	stringActionMap := so.getStringActionMap()
	intActionMap := so.getIntActionMap()
	longActionMap := so.getLongActionMap()
	type JsonSettings map[string]interface{}
	var jsonSettings JsonSettings
	if err := json.Unmarshal(data, &jsonSettings); err != nil {
		return err
	}
	for k := range jsonSettings {
		if bf, isBool := boolActionMap[k]; isBool {
			if v, hasVal := jsonSettings[k]; hasVal {
				bf(v.(bool), settings)
			} else {
				gofind.Log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if sf, isString := stringActionMap[k]; isString {
			if v, hasVal := jsonSettings[k]; hasVal {
				switch v := v.(type) {
				case string:
					sf(v, settings)
				case int:
					sf(strconv.Itoa(v), settings)
				case float32, float64:
					sf(fmt.Sprintf("%v", v.(float64)), settings)
				case []interface{}:
					for i := range v {
						sf(v[i].(string), settings)
					}
				default:
					gofind.Log(fmt.Sprintf("k: %v", k))
					gofind.Log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					gofind.Log(errMsg)
					return fmt.Errorf(errMsg)
				}
			} else {
				gofind.Log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if iff, isInt := intActionMap[k]; isInt {
			if v, hasVal := jsonSettings[k]; hasVal {
				iff(v.(int), settings)
			} else {
				gofind.Log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if lff, isLong := longActionMap[k]; isLong {
			if v, hasVal := jsonSettings[k]; hasVal {
				lff(v.(int64), settings)
			} else {
				gofind.Log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else {
			return fmt.Errorf("Invalid option: %s", k)
		}
	}
	return nil
}

func (so *SearchOptions) SearchSettingsFromArgs(args []string) (*SearchSettings, error) {
	settings := GetDefaultSearchSettings()
	// default printFiles to true since running as cli
	settings.SetPrintResults(true)
	boolActionMap := so.getBoolActionMap()
	stringActionMap := so.getStringActionMap()
	intActionMap := so.getIntActionMap()
	longActionMap := so.getLongActionMap()

	if false {
		gofind.Log(fmt.Sprintf("boolActionMap: %v", boolActionMap))
		gofind.Log(fmt.Sprintf("stringActionMap: %v", stringActionMap))
		gofind.Log(fmt.Sprintf("intActionMap: %v", intActionMap))
		gofind.Log(fmt.Sprintf("longActionMap: %v", longActionMap))
	}

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			k := strings.TrimLeft(args[i], "-")
			if false {
				gofind.Log(fmt.Sprintf("k: %s\n", k))
			}
			if bf, isBool := boolActionMap[k]; isBool {
				bf(true, settings)
			} else {
				i++
				if len(args) < i+1 {
					return nil, fmt.Errorf("Missing value for option: %s", k)
				}
				val := args[i]

				if sf, isString := stringActionMap[k]; isString {
					sf(val, settings)
				} else if iff, isInt := intActionMap[k]; isInt {
					intVal, err := strconv.Atoi(val)
					if err != nil {
						return nil, fmt.Errorf("Invalid value for option %s", k)
					}
					iff(intVal, settings)
				} else if lff, isLong := longActionMap[k]; isLong {
					longVal, err := strconv.ParseInt(val, 0, 64)
					if err != nil {
						return nil, fmt.Errorf("Invalid value for option %s", k)
					}
					lff(longVal, settings)
				} else {
					return nil, fmt.Errorf("Invalid option: %s", k)
				}
			}
		} else {
			settings.AddPath(args[i])
		}
		i++
	}
	if settings.Debug() {
		settings.SetVerbose(true)
	}
	return settings, nil
}

func (so *SearchOptions) getUsageString() string {
	var buffer bytes.Buffer
	buffer.WriteString("\nUsage:\n")
	buffer.WriteString(" gosearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n")
	sortKeyMap := so.getSortKeyMap()
	optStringMap := so.getOptStringMap()
	optDescMap := so.getOptDescMap()
	sortedKeys := gofind.GetSortedKeys(sortKeyMap)
	optStrings := gofind.GetMapValues(optStringMap)
	longestLen := gofind.GetLongestLen(optStrings)
	optFormat := fmt.Sprintf(" %%-%ds  %%s\n", longestLen)
	for _, k := range sortedKeys {
		o := optStringMap[sortKeyMap[k]]
		d := optDescMap[sortKeyMap[k]]
		buffer.WriteString(fmt.Sprintf(optFormat, o, d))
	}
	return buffer.String()
}

func (so *SearchOptions) PrintUsage() {
	gofind.Log(so.getUsageString())
	os.Exit(0)
}

func (so *SearchOptions) PrintVersion() {
	config := NewSearchConfig()
	gofind.Log(fmt.Sprintf("xsearch version %s", config.VERSION))
	os.Exit(0)
}

func (so *SearchOptions) getSortKeyMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.SearchOptions {
		sortKey := ""
		if o.Short == "" {
			sortKey = strings.ToLower(o.Long)
		} else {
			sortKey = fmt.Sprintf("%s@%s", strings.ToLower(o.Short),
				strings.ToLower(o.Long))
		}
		m[sortKey] = o.Long
	}
	return m
}

func (so *SearchOptions) getOptStringMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.SearchOptions {
		optString := ""
		if o.Short != "" {
			optString = fmt.Sprintf("-%s,", o.Short)
		}
		optString = fmt.Sprintf("%s--%s", optString, o.Long)
		m[o.Long] = optString
	}
	return m
}

func (so *SearchOptions) getOptDescMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.SearchOptions {
		m[o.Long] = o.Desc
	}
	return m
}

type boolAction func(b bool, settings *SearchSettings)

func (so *SearchOptions) getBoolActionMap() map[string]boolAction {
	m := map[string]boolAction{
		"allmatches": func(b bool, settings *SearchSettings) {
			settings.SetFirstMatch(!b)
		},
		"archivesonly": func(b bool, settings *SearchSettings) {
			settings.SetArchivesOnly(b)
		},
		"colorize": func(b bool, settings *SearchSettings) {
			settings.SetColorize(b)
		},
		"debug": func(b bool, settings *SearchSettings) {
			settings.SetDebug(b)
		},
		"excludehidden": func(b bool, settings *SearchSettings) {
			settings.SetIncludeHidden(!b)
		},
		"firstmatch": func(b bool, settings *SearchSettings) {
			settings.SetFirstMatch(b)
		},
		"followsymlinks": func(b bool, settings *SearchSettings) {
			settings.SetFollowSymlinks(b)
		},
		"help": func(b bool, settings *SearchSettings) {
			settings.SetPrintUsage(b)
		},
		"includehidden": func(b bool, settings *SearchSettings) {
			settings.SetIncludeHidden(b)
		},
		"multilinesearch": func(b bool, settings *SearchSettings) {
			settings.SetMultiLineSearch(b)
		},
		"nocolorize": func(b bool, settings *SearchSettings) {
			settings.SetColorize(!b)
		},
		"nofollowsymlinks": func(b bool, settings *SearchSettings) {
			settings.SetFollowSymlinks(!b)
		},
		"noprintdirs": func(b bool, settings *SearchSettings) {
			settings.SetPrintDirs(!b)
		},
		"noprintfiles": func(b bool, settings *SearchSettings) {
			settings.SetPrintFiles(!b)
		},
		"noprintlines": func(b bool, settings *SearchSettings) {
			settings.SetPrintLines(!b)
		},
		"noprintmatches": func(b bool, settings *SearchSettings) {
			settings.SetPrintResults(!b)
		},
		"norecursive": func(b bool, settings *SearchSettings) {
			settings.SetRecursive(!b)
		},
		"nosearcharchives": func(b bool, settings *SearchSettings) {
			settings.SetSearchArchives(!b)
		},
		"printdirs": func(b bool, settings *SearchSettings) {
			settings.SetPrintDirs(b)
		},
		"printfiles": func(b bool, settings *SearchSettings) {
			settings.SetPrintFiles(b)
		},
		"printlines": func(b bool, settings *SearchSettings) {
			settings.SetPrintLines(b)
		},
		"printmatches": func(b bool, settings *SearchSettings) {
			settings.SetPrintResults(b)
		},
		"recursive": func(b bool, settings *SearchSettings) {
			settings.SetRecursive(b)
		},
		"searcharchives": func(b bool, settings *SearchSettings) {
			settings.SetSearchArchives(b)
		},
		"sort-ascending": func(b bool, settings *SearchSettings) {
			settings.SetSortDescending(!b)
		},
		"sort-caseinsensitive": func(b bool, settings *SearchSettings) {
			settings.SetSortCaseInsensitive(b)
		},
		"sort-casesensitive": func(b bool, settings *SearchSettings) {
			settings.SetSortCaseInsensitive(!b)
		},
		"sort-descending": func(b bool, settings *SearchSettings) {
			settings.SetSortDescending(b)
		},
		"uniquelines": func(b bool, settings *SearchSettings) {
			settings.SetUniqueLines(b)
		},
		"verbose": func(b bool, settings *SearchSettings) {
			settings.SetVerbose(b)
		},
		"version": func(b bool, settings *SearchSettings) {
			settings.SetPrintVersion(b)
		},
	}
	for _, o := range so.SearchOptions {
		if o.Short != "" {
			if f, ok := m[o.Long]; ok {
				m[o.Short] = f
			}
		}
	}
	return m
}

type stringAction func(s string, settings *SearchSettings)

func (so *SearchOptions) getStringActionMap() map[string]stringAction {
	m := map[string]stringAction{
		"encoding": func(s string, settings *SearchSettings) {
			settings.SetTextFileEncoding(s)
		},
		"in-archiveext": func(s string, settings *SearchSettings) {
			settings.AddInArchiveExtension(s)
		},
		"in-archivefilepattern": func(s string, settings *SearchSettings) {
			settings.AddInArchiveFilePattern(s)
		},
		"in-dirpattern": func(s string, settings *SearchSettings) {
			settings.AddInDirPattern(s)
		},
		"in-ext": func(s string, settings *SearchSettings) {
			settings.AddInExtension(s)
		},
		"in-filepattern": func(s string, settings *SearchSettings) {
			settings.AddInFilePattern(s)
		},
		"in-filetype": func(s string, settings *SearchSettings) {
			settings.AddInFileType(gofind.GetFileTypeForName(s))
		},
		"in-linesafterpattern": func(s string, settings *SearchSettings) {
			settings.AddInLinesAfterPattern(s)
		},
		"in-linesbeforepattern": func(s string, settings *SearchSettings) {
			settings.AddInLinesBeforePattern(s)
		},
		"linesaftertopattern": func(s string, settings *SearchSettings) {
			settings.AddLinesAfterToPattern(s)
		},
		"linesafteruntilpattern": func(s string, settings *SearchSettings) {
			settings.AddLinesAfterUntilPattern(s)
		},
		"maxlastmod": func(s string, settings *SearchSettings) {
			settings.SetMaxLastModFromString(s)
		},
		"minlastmod": func(s string, settings *SearchSettings) {
			settings.SetMinLastModFromString(s)
		},
		"out-archiveext": func(s string, settings *SearchSettings) {
			settings.AddOutArchiveExtension(s)
		},
		"out-archivefilepattern": func(s string, settings *SearchSettings) {
			settings.AddOutArchiveFilePattern(s)
		},
		"out-dirpattern": func(s string, settings *SearchSettings) {
			settings.AddOutDirPattern(s)
		},
		"out-ext": func(s string, settings *SearchSettings) {
			settings.AddOutExtension(s)
		},
		"out-filepattern": func(s string, settings *SearchSettings) {
			settings.AddOutFilePattern(s)
		},
		"out-filetype": func(s string, settings *SearchSettings) {
			settings.AddOutFileType(gofind.GetFileTypeForName(s))
		},
		"out-linesafterpattern": func(s string, settings *SearchSettings) {
			settings.AddOutLinesAfterPattern(s)
		},
		"out-linesbeforepattern": func(s string, settings *SearchSettings) {
			settings.AddOutLinesBeforePattern(s)
		},
		"path": func(s string, settings *SearchSettings) {
			settings.AddPath(s)
		},
		"searchpattern": func(s string, settings *SearchSettings) {
			settings.AddSearchPattern(s)
		},
		"settings-file": func(s string, settings *SearchSettings) {
			err := so.SettingsFromFile(s, settings)
			if err != nil {
				return
			}
		},
		"sort-by": func(s string, settings *SearchSettings) {
			settings.SetSortByFromString(s)
		},
	}
	for _, o := range so.SearchOptions {
		if o.Short != "" {
			if f, ok := m[o.Long]; ok {
				m[o.Short] = f
			}
		}
	}
	return m
}

type intAction func(i int, settings *SearchSettings)

func (so *SearchOptions) getIntActionMap() map[string]intAction {
	m := map[string]intAction{
		"linesafter": func(i int, settings *SearchSettings) {
			settings.SetLinesAfter(i)
		},
		"linesbefore": func(i int, settings *SearchSettings) {
			settings.SetLinesBefore(i)
		},
		"maxdepth": func(i int, settings *SearchSettings) {
			settings.SetMaxDepth(i)
		},
		"maxlinelength": func(i int, settings *SearchSettings) {
			settings.SetMaxLineLength(i)
		},
		"mindepth": func(i int, settings *SearchSettings) {
			settings.SetMinDepth(i)
		},
	}
	return m
}

type longAction func(l int64, settings *SearchSettings)

func (so *SearchOptions) getLongActionMap() map[string]longAction {
	m := map[string]longAction{
		"maxsize": func(l int64, settings *SearchSettings) {
			settings.SetMaxSize(l)
		},
		"minsize": func(l int64, settings *SearchSettings) {
			settings.SetMinSize(l)
		},
	}
	return m
}

func (so *SearchOptions) generateCodeFile(filePath string) {
	var buffer bytes.Buffer
	depth := 0
	buffer.WriteString("package gosearch\n\n")
	buffer.WriteString("func GetSearchOptions() *SearchOptions {\n")
	depth++
	buffer.WriteString(fmt.Sprintf("%sreturn &SearchOptions{\n", strings.Repeat("\t", depth)))
	depth++
	buffer.WriteString(fmt.Sprintf("%s[]*SearchOption{\n", strings.Repeat("\t", depth)))
	depth++
	for _, so := range so.SearchOptions {
		buffer.WriteString(fmt.Sprintf("%s{\"%s\", \"%s\", \"%s\"},\n",
			strings.Repeat("\t", depth), so.Short, so.Long, gofind.EscapeQuotes(so.Desc)))
	}
	depth--
	buffer.WriteString(fmt.Sprintf("%s},\n", strings.Repeat("\t", depth)))
	depth--
	buffer.WriteString(fmt.Sprintf("%s}\n}\n", strings.Repeat("\t", depth)))
	err := os.WriteFile(filePath, buffer.Bytes(), 0644)
	if err != nil {
		panic(err)
	}
}
