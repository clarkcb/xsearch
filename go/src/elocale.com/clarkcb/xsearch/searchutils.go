package xsearch

import (
	"encoding/xml"
	"os"
	"sort"
)

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

func union(s1, s2 set) set {
	s := make(map[string]bool)
	for k, _ := range s1 {
		s[k] = true
	}
	for k, _ := range s2 {
		s[k] = true
	}
	return set(s)
}

func contains(slice []string, s string) bool {
	for _, as := range slice {
		if s == as {
			return true
		}
	}
	return false
}

func getLongestLen(slice []string) int {
	longestLen := 0
	for _, s := range slice {
		if len(s) > longestLen {
			longestLen = len(s)
		}
	}
	return longestLen
}

func getMapKeys(m map[string]string) []string {
	keys := []string{}
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func getMapValues(m map[string]string) []string {
	values := []string{}
	for _, v := range m {
		values = append(values, v)
	}
	return values
}

func getSortedKeys(m map[string]string) []string {
	keys := getMapKeys(m)
	sort.Strings(keys)
	return keys
}

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
