package gosearch

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
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
	data, err := ioutil.ReadFile(SEARCHOPTIONSPATH)
	if err != nil {
		return &SearchOptions{}, err
	}
	var searchOptions SearchOptions
	if err = json.Unmarshal(data, &searchOptions); err != nil {
		return &SearchOptions{}, err
	}
	return &searchOptions, nil
}

func NewSearchOptions() *SearchOptions {
	searchOptions, err := SearchOptionsFromJson()
	if err != nil {
		// do something
	}
	return searchOptions
}

func (so *SearchOptions) SettingsFromFile(filepath string, settings *SearchSettings) error {
	if data, err := ioutil.ReadFile(filepath); err != nil {
		return err
	} else {
		return so.SettingsFromJson(data, settings)
	}
}

func (so *SearchOptions) SettingsFromJson(data []byte, settings *SearchSettings) error {
	argActionMap := so.getArgActionMap()
	boolFlagActionMap := so.getBoolFlagActionMap()
	type JsonSettings map[string]interface{}
	var jsonSettings JsonSettings
	if err := json.Unmarshal(data, &jsonSettings); err != nil {
		return err
	}
	for k := range jsonSettings {
		if af, isAction := argActionMap[k]; isAction {
			if v, hasVal := jsonSettings[k]; hasVal {
				switch v := v.(type) {
				case string:
					af(v, settings)
				case int:
					af(strconv.Itoa(v), settings)
				case float32, float64:
					af(fmt.Sprintf("%v", v.(float64)), settings)
				case []interface{}:
					for i := range v {
						af(v[i].(string), settings)
					}
				default:
					log(fmt.Sprintf("k: %v", k))
					log(fmt.Sprintf("reflect.TypeOf(v).Kind(): %v", reflect.TypeOf(v).Kind()))
					errMsg := fmt.Sprintf("Unknown data type in settings file")
					log(errMsg)
					return fmt.Errorf(errMsg)
				}
			} else {
				log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if ff, isFlag := boolFlagActionMap[k]; isFlag {
			if v, hasVal := jsonSettings[k]; hasVal {
				ff(v.(bool), settings)
			} else {
				log(fmt.Sprintf("value for %v is invalid", k))
			}
		} else if k == "path" {
			if sp, hasStartPath := jsonSettings[k]; hasStartPath {
				settings.AddPath(sp.(string))
			} else {
				log("startpath value is invalid")
			}
		} else {
			return fmt.Errorf("Invalid option: %s", k)
		}

	}
	return nil
}

func (so *SearchOptions) SearchSettingsFromArgs(args []string) (*SearchSettings, error) {
	settings := GetDefaultSearchSettings()
	argActionMap := so.getArgActionMap()
	flagActionMap := so.getBoolFlagActionMap()

	if false {
		log(fmt.Sprintf("argActionMap: %v", argActionMap))
		log(fmt.Sprintf("flagActionMap: %v", flagActionMap))
	}

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			k := strings.TrimLeft(args[i], "-")
			if false {
				log(fmt.Sprintf("k: %s\n", k))
			}
			if af, isAction := argActionMap[k]; isAction {
				i++
				if len(args) < i+1 {
					return nil, fmt.Errorf("Missing value for option: %s", k)
				}
				val := args[i]
				af(val, settings)
			} else if ff, isFlag := flagActionMap[k]; isFlag {
				ff(true, settings)
			} else {
				return nil, fmt.Errorf("Invalid option: %s", k)
			}
		} else {
			settings.AddPath(args[i])
		}
		i++
	}
	if settings.Debug {
		settings.Verbose = true
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
	sortedKeys := getSortedKeys(sortKeyMap)
	optStrings := getMapValues(optStringMap)
	longestLen := getLongestLen(optStrings)
	optFormat := fmt.Sprintf(" %%-%ds  %%s\n", longestLen)
	for _, k := range sortedKeys {
		o := optStringMap[sortKeyMap[k]]
		d := optDescMap[sortKeyMap[k]]
		buffer.WriteString(fmt.Sprintf(optFormat, o, d))
	}
	return buffer.String()
}

func (so *SearchOptions) PrintUsage() {
	log(so.getUsageString())
	os.Exit(0)
}

func (so *SearchOptions) PrintVersion() {
	log(fmt.Sprintf("xsearch version %s", VERSION))
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

type argAction func(s string, settings *SearchSettings)

func (so *SearchOptions) getArgActionMap() map[string]argAction {
	m := map[string]argAction{
		"encoding": func(s string, settings *SearchSettings) {
			settings.TextFileEncoding = s
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
			settings.AddInFileType(getFileTypeForName(s))
		},
		"in-linesafterpattern": func(s string, settings *SearchSettings) {
			settings.AddInLinesAfterPattern(s)
		},
		"in-linesbeforepattern": func(s string, settings *SearchSettings) {
			settings.AddInLinesBeforePattern(s)
		},
		"linesafter": func(s string, settings *SearchSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.LinesAfter = num
			} else {
				log(fmt.Sprintf("Invalid value for linesafter: %s\n", s))
			}
		},
		"linesaftertopattern": func(s string, settings *SearchSettings) {
			settings.AddLinesAfterToPattern(s)
		},
		"linesafteruntilpattern": func(s string, settings *SearchSettings) {
			settings.AddLinesAfterUntilPattern(s)
		},
		"linesbefore": func(s string, settings *SearchSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.LinesBefore = num
			} else {
				log(fmt.Sprintf("Invalid value for linesbefore: %s\n", s))
			}
		},
		"maxlinelength": func(s string, settings *SearchSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.MaxLineLength = num
			} else {
				log(fmt.Sprintf("Invalid value for maxlinelength: %s\n", s))
			}
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
			settings.AddOutFileType(getFileTypeForName(s))
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
			so.SettingsFromFile(s, settings)
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

type boolFlagAction func(b bool, settings *SearchSettings)

func (so *SearchOptions) getBoolFlagActionMap() map[string]boolFlagAction {
	m := map[string]boolFlagAction{
		"allmatches": func(b bool, settings *SearchSettings) {
			settings.FirstMatch = !b
		},
		"archivesonly": func(b bool, settings *SearchSettings) {
			settings.SetArchivesOnly(b)
		},
		"debug": func(b bool, settings *SearchSettings) {
			settings.SetDebug(b)
		},
		"excludehidden": func(b bool, settings *SearchSettings) {
			settings.ExcludeHidden = b
		},
		"firstmatch": func(b bool, settings *SearchSettings) {
			settings.FirstMatch = b
		},
		"help": func(b bool, settings *SearchSettings) {
			settings.PrintUsage = b
		},
		"includehidden": func(b bool, settings *SearchSettings) {
			settings.ExcludeHidden = !b
		},
		"listdirs": func(b bool, settings *SearchSettings) {
			settings.ListDirs = b
		},
		"listfiles": func(b bool, settings *SearchSettings) {
			settings.ListFiles = b
		},
		"listlines": func(b bool, settings *SearchSettings) {
			settings.ListLines = b
		},
		"multilinesearch": func(b bool, settings *SearchSettings) {
			settings.MultiLineSearch = b
		},
		"noprintmatches": func(b bool, settings *SearchSettings) {
			settings.PrintResults = !b
		},
		"norecursive": func(b bool, settings *SearchSettings) {
			settings.Recursive = !b
		},
		"nosearcharchives": func(b bool, settings *SearchSettings) {
			settings.SearchArchives = !b
		},
		"printmatches": func(b bool, settings *SearchSettings) {
			settings.PrintResults = b
		},
		"recursive": func(b bool, settings *SearchSettings) {
			settings.Recursive = b
		},
		"searcharchives": func(b bool, settings *SearchSettings) {
			settings.SearchArchives = b
		},
		"uniquelines": func(b bool, settings *SearchSettings) {
			settings.UniqueLines = b
		},
		"verbose": func(b bool, settings *SearchSettings) {
			settings.Verbose = b
		},
		"version": func(b bool, settings *SearchSettings) {
			settings.PrintVersion = b
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
