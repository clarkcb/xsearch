package xsearch

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type SearchOption struct {
	Short string
	Long  string
	Desc  string
}

type SearchOptions struct {
	SearchOptions []SearchOption
}

func NewSearchOptions() *SearchOptions {
	searchOptions, err := searchOptionsFromXml()
	if err != nil {
		panic(err.Error())
	}
	return searchOptions
}

func (so *SearchOptions) SearchSettingsFromArgs(args []string) (*SearchSettings, error) {
	settings := GetDefaultSearchSettings()
	argActionMap := so.getArgActionMap()
	flagActionMap := so.getFlagActionMap()

	if false {
		fmt.Printf("argActionMap: %v", argActionMap)
		fmt.Printf("flagActionMap: %v", flagActionMap)
	}

	for i := 0; i < len(args); {
		if strings.HasPrefix(args[i], "-") {
			k := strings.TrimLeft(args[i], "-")
			if false {
				fmt.Printf("k: %s\n", k)
			}
			if af, isAction := argActionMap[k]; isAction {
				i++
				if len(args) < i+1 {
					return nil, fmt.Errorf("Missing value for option: %s", k)
				}
				val := args[i]
				af(val, settings)
			} else if ff, isFlag := flagActionMap[k]; isFlag {
				ff(settings)
			} else {
				return nil, fmt.Errorf("Unknown option: %s", k)
			}
		} else {
			settings.StartPath = args[i]
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
	buffer.WriteString("  gosearch [options] <startpath>\n\nOptions:\n")
	sortKeyMap := so.getSortKeyMap()
	optStringMap := so.getOptStringMap()
	optDescMap := so.getOptDescMap()
	sortedKeys := getSortedKeys(sortKeyMap)
	optStrings := getMapValues(optStringMap)
	longestLen := getLongestLen(optStrings)
	optFormat := fmt.Sprintf("  %%-%ds  %%s\n", longestLen)
	for _, k := range sortedKeys {
		o := optStringMap[sortKeyMap[k]]
		d := optDescMap[sortKeyMap[k]]
		buffer.WriteString(fmt.Sprintf(optFormat, o, d))
	}
	return buffer.String()
}

func (so *SearchOptions) PrintUsage() {
	fmt.Println(so.getUsageString())
	os.Exit(0)
}

func (so *SearchOptions) getSortKeyMap() map[string]string {
	m := map[string]string{}
	for _, o := range so.SearchOptions {
		sortKey := ""
		if o.Short != "" {
			sortKey = strings.ToLower(o.Short)
		}
		sortKey = fmt.Sprintf("%s%s", sortKey, strings.ToLower(o.Long))
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
		"in-dirpattern": func(s string, settings *SearchSettings) {
			settings.AddInDirPattern(s)
		},
		"in-ext": func(s string, settings *SearchSettings) {
			settings.AddInExtension(s)
		},
		"in-filepattern": func(s string, settings *SearchSettings) {
			settings.AddInFilePattern(s)
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
				fmt.Printf("Invalid value for linesafter: %s\n", s)
			}
		},
		"linesbefore": func(s string, settings *SearchSettings) {
			num, err := strconv.Atoi(s)
			if err == nil {
				settings.LinesBefore = num
			} else {
				fmt.Printf("Invalid value for linesbefore: %s\n", s)
			}
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
		"out-linesafterpattern": func(s string, settings *SearchSettings) {
			settings.AddOutLinesAfterPattern(s)
		},
		"out-linesbeforepattern": func(s string, settings *SearchSettings) {
			settings.AddOutLinesBeforePattern(s)
		},
		"search": func(s string, settings *SearchSettings) {
			settings.AddSearchPattern(s)
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

type flagAction func(settings *SearchSettings)

func (so *SearchOptions) getFlagActionMap() map[string]flagAction {
	m := map[string]flagAction{
		"allmatches": func(settings *SearchSettings) {
			settings.FirstMatch = false
		},
		"debug": func(settings *SearchSettings) {
			settings.Debug = true
		},
		"dotiming": func(settings *SearchSettings) {
			settings.DoTiming = true
		},
		"firstmatch": func(settings *SearchSettings) {
			settings.FirstMatch = true
		},
		"help": func(settings *SearchSettings) {
			settings.PrintUsage = true
		},
		"listdirs": func(settings *SearchSettings) {
			settings.ListDirs = true
		},
		"listfiles": func(settings *SearchSettings) {
			settings.ListFiles = true
		},
		"listlines": func(settings *SearchSettings) {
			settings.ListLines = true
		},
		"multilinesearch": func(settings *SearchSettings) {
			settings.MultiLineSearch = true
		},
		"noprintmatches": func(settings *SearchSettings) {
			settings.PrintResults = false
		},
		"nosearchcompressed": func(settings *SearchSettings) {
			settings.SearchCompressed = false
		},
		"printmatches": func(settings *SearchSettings) {
			settings.PrintResults = true
		},
		"searchcompressed": func(settings *SearchSettings) {
			settings.SearchCompressed = true
		},
		"verbose": func(settings *SearchSettings) {
			settings.Verbose = true
		},
		"version": func(settings *SearchSettings) {
			settings.PrintVersion = true
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

type XmlSearchOptions struct {
	SearchOptions []XmlSearchOption `xml:"searchoption"`
}

type XmlSearchOption struct {
	Short string `xml:"short,attr"`
	Long  string `xml:"long,attr"`
	Desc  string `xml:",chardata"`
}

const searchOptionsXmlPath = "/Users/cary/src/git/xsearch/shared/searchoptions.xml"

func searchOptionsFromXml() (*SearchOptions, error) {
	var searchOptions []SearchOption
	xmlSearchOptions := &XmlSearchOptions{}

	if err := loadXmlFile(searchOptionsXmlPath, xmlSearchOptions); err != nil {
		return nil, err
	}

	for _, x := range xmlSearchOptions.SearchOptions {
		searchOption := &SearchOption{x.Short, x.Long, strings.TrimSpace(x.Desc)}
		searchOptions = append(searchOptions, *searchOption)
	}
	return &SearchOptions{searchOptions}, nil
}
