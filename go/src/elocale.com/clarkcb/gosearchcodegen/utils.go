package gosearchcodegen

import (
	"encoding/xml"
	"os"
	"sort"
)

func loadXmlFile(xmlFilePath string, targetStruct interface{}) error {
	file, err := os.Open(xmlFilePath)
	if err != nil {
		panic(err.Error())
	}

	defer func() {
		err := file.Close()
		if err != nil {
			panic(err.Error())
		}
	}()

	decoder := xml.NewDecoder(file)

	if err := decoder.Decode(targetStruct); err != nil {
		return err
	}
	return nil
}

type set map[string]bool

func makeSet(slice []string) set {
	s := make(map[string]bool)
	for _, v := range slice {
		if v != "" {
			s[v] = true
		}
	}
	return set(s)
}

func getSortedValues(s set) []string {
	keys := []string{}
	for k := range s {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
