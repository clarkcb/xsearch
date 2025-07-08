/*******************************************************************************
SearchResultFormatter

Class to provide formatting of search result instances

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2020
*******************************************************************************/

package javasearch;

import javafind.SortBy;

import java.util.Comparator;
import java.util.List;

public class SearchResultSorter {
    private final SearchSettings settings;

    public SearchResultSorter(final SearchSettings settings) {
        this.settings = settings;
    }

    public SearchSettings getSettings() {
        return settings;
    }

    public final Comparator<SearchResult> getSearchResultComparator() {
        var sortBy = settings.getSortBy() == null ? SortBy.FILEPATH : settings.getSortBy();
        if (settings.getSortDescending()) {
            return switch (sortBy) {
                case FILENAME -> (SearchResult fr1, SearchResult fr2) -> fr2.compareByName(fr1, settings.getSortCaseInsensitive());
                case FILESIZE -> (SearchResult fr1, SearchResult fr2) -> fr2.compareBySize(fr1, settings.getSortCaseInsensitive());
                case FILETYPE -> (SearchResult fr1, SearchResult fr2) -> fr2.compareByType(fr1, settings.getSortCaseInsensitive());
                case LASTMOD -> (SearchResult fr1, SearchResult fr2) -> fr2.compareByLastMod(fr1, settings.getSortCaseInsensitive());
                default -> (SearchResult fr1, SearchResult fr2) -> fr2.compareByPath(fr1, settings.getSortCaseInsensitive());
            };
        }
        return switch (sortBy) {
            case FILENAME -> (SearchResult fr1, SearchResult fr2) -> fr1.compareByName(fr2, settings.getSortCaseInsensitive());
            case FILESIZE -> (SearchResult fr1, SearchResult fr2) -> fr1.compareBySize(fr2, settings.getSortCaseInsensitive());
            case FILETYPE -> (SearchResult fr1, SearchResult fr2) -> fr1.compareByType(fr2, settings.getSortCaseInsensitive());
            case LASTMOD -> (SearchResult fr1, SearchResult fr2) -> fr1.compareByLastMod(fr2, settings.getSortCaseInsensitive());
            default -> (SearchResult fr1, SearchResult fr2) -> fr1.compareByPath(fr2, settings.getSortCaseInsensitive());
        };
    }

    public final void sort(List<SearchResult> searchResults) {
        if (searchResults.isEmpty()) {
            return;
        }
        var searchResultComparator = getSearchResultComparator();
        searchResults.sort(searchResultComparator);
    }
}
