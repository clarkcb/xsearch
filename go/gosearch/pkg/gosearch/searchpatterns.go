package gosearch

import (
	"regexp"
)

type SearchPatternsIterator struct {
	idx      int
	patterns *SearchPatterns
}

func NewSearchPatternsIterator(sp *SearchPatterns) *SearchPatternsIterator {
	return &SearchPatternsIterator{
		-1,
		sp,
	}
}

func (i *SearchPatternsIterator) Next() bool {
	i.idx++
	if i.idx >= len(i.patterns.patterns) {
		return false
	}
	return true
}

func (i *SearchPatternsIterator) Value() *regexp.Regexp {
	return i.patterns.patterns[i.idx]
}

type SearchPatterns struct {
	patterns []*regexp.Regexp
}

func NewSearchPatterns() *SearchPatterns {
	return &SearchPatterns{
		[]*regexp.Regexp{},
	}
}

func (sp *SearchPatterns) AddPattern(s *string) {
	sp.patterns = append(sp.patterns, regexp.MustCompile(*s))
}

func (sp *SearchPatterns) IsEmpty() bool {
	return len(sp.patterns) == 0
}

func (sp *SearchPatterns) Iterator() *SearchPatternsIterator {
	return NewSearchPatternsIterator(sp)
}

func (sp *SearchPatterns) MatchesAny(s *string) bool {
	for _, p := range sp.patterns {
		if p.MatchString(*s) {
			return true
		}
	}
	return false
}

func (sp *SearchPatterns) AnyMatchesAny(ss []*string) bool {
	for _, s := range ss {
		if sp.MatchesAny(s) {
			return true
		}
	}
	return false
}
