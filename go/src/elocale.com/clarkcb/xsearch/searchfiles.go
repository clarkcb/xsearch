package xsearch

import (
	"bytes"
	"path/filepath"
	"strings"
)

type SearchItemsIterator struct {
	idx   int
	items *SearchItems
}

func NewSearchItemsIterator(sf *SearchItems) *SearchItemsIterator {
	return &SearchItemsIterator{
		-1,
		sf,
	}
}

func (i *SearchItemsIterator) Next() bool {
	i.idx++
	if i.idx >= len(i.items.items) {
		return false
	}
	return true
}

func (i *SearchItemsIterator) Value() *SearchItem {
	return i.items.items[i.idx]
}

type SearchItems struct {
	items     []*SearchItem
	strPtrMap map[string]*string
}

func NewSearchItems() *SearchItems {
	return &SearchItems{
		[]*SearchItem{},
		make(map[string]*string),
	}
}

// limits string pointers to one per distinct string (memory management)
func (si *SearchItems) getStrPtr(s *string) *string {
	strPtr := s
	if sp, ok := si.strPtrMap[*s]; ok {
		strPtr = sp
	} else {
		si.strPtrMap[*s] = s
	}
	return strPtr
}

func (si *SearchItems) AddItem(i *SearchItem) {
	si.items = append(si.items, &SearchItem{
		i.Containers,
		si.getStrPtr(i.Path),
		si.getStrPtr(i.Name),
	})
}

func (si *SearchItems) Count() int {
	return len(si.items)
}

func (si *SearchItems) IsEmpty() bool {
	return len(si.items) == 0
}

func (si *SearchItems) Iterator() *SearchItemsIterator {
	return NewSearchItemsIterator(si)
}

type SearchItem struct {
	Containers []string
	Path       *string
	Name       *string
}

func NewSearchItem(path *string, name *string) *SearchItem {
	return &SearchItem{
		[]string{},
		path,
		name,
	}
}

func (si *SearchItem) AddContainer(c string) {
	si.Containers = append(si.Containers, c)
}

func (si *SearchItem) String() string {
	var buffer bytes.Buffer
	if len(si.Containers) > 0 {
		buffer.WriteString(strings.Join(si.Containers, "::"))
		buffer.WriteString("::")
	}
	buffer.WriteString(filepath.Join(*si.Path, *si.Name))
	return buffer.String()
}
