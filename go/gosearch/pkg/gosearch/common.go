package gosearch

import (
	"fmt"
	"sort"

	"github.com/pmylund/sortutil"
)

func log(message string) {
	fmt.Println(message)
}

type set map[string]bool

func makeSet(slice []string) set {
	s := make(map[string]bool)
	for _, v := range slice {
		if v != "" {
			s[v] = true
		}
	}
	return s
}

func makeMap(slice []string) map[string]string {
	s := make(map[string]string)
	for _, v := range slice {
		if v != "" {
			s[v] = v
		}
	}
	return s
}

func union(s1, s2 set) set {
	s := make(map[string]bool)
	for k, _ := range s1 {
		s[k] = true
	}
	for k, _ := range s2 {
		s[k] = true
	}
	return s
}

func contains(slice []string, s string) bool {
	for _, as := range slice {
		if s == as {
			return true
		}
	}
	return false
}

func containsFileType(fileTypes []FileType, fileType FileType) bool {
	for _, ft := range fileTypes {
		if fileType == ft {
			return true
		}
	}
	return false
}

func containsV(slice []string, s string) bool {
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
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func getMapValues(m map[string]string) []string {
	var values []string
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

func getCountMapKeys(m map[string]int) []string {
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func getSortedCountKeys(m map[string]int) []string {
	keys := getCountMapKeys(m)
	sort.Strings(keys)
	return keys
}

func getCaseInsensitiveSortedCountKeys(m map[string]int) []string {
	mk := make([]string, len(m))
	i := 0
	for k, _ := range m {
		mk[i] = k
		i++
	}
	sortutil.CiAsc(mk)
	return mk
}

func getHighestMapVal(m map[string]int) int {
	highestVal := 0
	for _, v := range m {
		if v > highestVal {
			highestVal = v
		}
	}
	return highestVal
}

func getNumLen(num int) int {
	numStr := fmt.Sprintf("%d", num)
	return len(numStr)
}

// just for fun, a strictly numeric way to get number length
func getNumLen2(num int) int {
	next, mult := 10, 10
	count, maxcount := 1, 10
	for count < maxcount {
		if num < next {
			return count
		}
		next = next * mult
		count++
	}
	return count
}

func getMaxInt(x int, y int) int {
	if x < y {
		return y
	}
	return x
}

func getMinInt(x int, y int) int {
	if x < y {
		return x
	}
	return y
}
