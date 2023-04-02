package gosearch

import (
	"bytes"
	"fmt"
	"os"
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

func (i *SearchItemsIterator) HasNext() bool {
	if i.idx >= len(i.items.items) {
		return false
	}
	return true
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

func (i *SearchItemsIterator) Take(count int) []*SearchItem {
	if i.idx < 0 {
		i.idx = 0
	}
	maxCount := getMinInt(count, len(i.items.items)-i.idx)
	searchItems := i.items.items[i.idx : maxCount+i.idx]
	i.idx += maxCount
	return searchItems
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
		i.Path,
		i.Name,
		i.fileType,
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
	Path       string
	Name       string
	fileType   FileType
}

func NewSearchItem(path string, name string, fileType FileType) *SearchItem {
	return &SearchItem{
		[]string{},
		path,
		name,
		fileType,
	}
}

func (si *SearchItem) AddContainer(c string) {
	si.Containers = append(si.Containers, c)
}

const containerSeparator = "!"

func (si *SearchItem) String() string {
	var buffer bytes.Buffer
	if len(si.Containers) > 0 {
		buffer.WriteString(strings.Join(si.Containers, containerSeparator))
		buffer.WriteString(containerSeparator)
	}
	path := normalizePath(si.Path)
	if isDotDir(path) {
		buffer.WriteString(fmt.Sprintf("%s%c%s", path, os.PathSeparator, si.Name))
	} else {
		buffer.WriteString(filepath.Join(si.Path, si.Name))
	}
	return buffer.String()
}
