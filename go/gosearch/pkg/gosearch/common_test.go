package gosearch

import "testing"

func TestContains(t *testing.T) {
	greetings := []string{
		"hi", "hello", "g'day", "ahoy", "greetings", "hey",
	}

	others := []string{
		"привет", "здравствуйте", "こんにちは",
	}

	b := false
	for _, g := range greetings {
		if b = containsV(greetings, g); !b {
			t.Errorf("contains(greetings, \"%s\")=%t, expected=true", g, b)
		}
	}

	for _, g := range others {
		if b = containsV(greetings, g); b {
			t.Errorf("contains(greetings, \"%s\")=%t, expected=false", g, b)
		}
	}
}

func TestGetLongestLen(t *testing.T) {
	greetings := []string{
		"hi", "hello", "g'day", "ahoy", "greetings", "hey",
	}

	longestLen := getLongestLen(greetings)
	expectedLongestLen := 9

	if longestLen != expectedLongestLen {
		t.Errorf("longestLen: %d, expected=%d", longestLen, expectedLongestLen)
	}
}

func TestGetSortedKeys(t *testing.T) {
	greetings := []string{
		"hi", "hello", "g'day", "ahoy", "greetings", "hey",
	}
	greetingsMap := map[string]string{}
	for _, g := range greetings {
		greetingsMap[g] = g
	}

	sortedGreetings := getSortedKeys(greetingsMap)

	if sortedGreetings[0] != "ahoy" {
		t.Errorf("sortedGreetings[0]=\"%s\", expected=\"ahoy\"",
			sortedGreetings[0])
	}

	if sortedGreetings[len(sortedGreetings)-1] != "hi" {
		t.Errorf("sortedGreetings[%d]=\"%s\", expected=\"hi\"",
			len(sortedGreetings)-1, sortedGreetings[len(sortedGreetings)-1])
	}
}
