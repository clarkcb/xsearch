/*******************************************************************************
SearchResultSorter

Class to provide sorting of search result instances

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2025
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
                case FILENAME -> (SearchResult sr1, SearchResult sr2) -> sr2.compareByName(sr1, settings.getSortCaseInsensitive());
                case FILESIZE -> (SearchResult sr1, SearchResult sr2) -> sr2.compareBySize(sr1, settings.getSortCaseInsensitive());
                case FILETYPE -> (SearchResult sr1, SearchResult sr2) -> sr2.compareByType(sr1, settings.getSortCaseInsensitive());
                case LASTMOD -> (SearchResult sr1, SearchResult sr2) -> sr2.compareByLastMod(sr1, settings.getSortCaseInsensitive());
                default -> (SearchResult sr1, SearchResult sr2) -> sr2.compareByPath(sr1, settings.getSortCaseInsensitive());
            };
        }
        return switch (sortBy) {
            case FILENAME -> (SearchResult sr1, SearchResult sr2) -> sr1.compareByName(sr2, settings.getSortCaseInsensitive());
            case FILESIZE -> (SearchResult sr1, SearchResult sr2) -> sr1.compareBySize(sr2, settings.getSortCaseInsensitive());
            case FILETYPE -> (SearchResult sr1, SearchResult sr2) -> sr1.compareByType(sr2, settings.getSortCaseInsensitive());
            case LASTMOD -> (SearchResult sr1, SearchResult sr2) -> sr1.compareByLastMod(sr2, settings.getSortCaseInsensitive());
            default -> (SearchResult sr1, SearchResult sr2) -> sr1.compareByPath(sr2, settings.getSortCaseInsensitive());
        };
    }

    public final void sort(List<SearchResult> searchResults) {
        if (searchResults.size() < 2) {
            return;
        }
        var searchResultComparator = getSearchResultComparator();
        searchResults.sort(searchResultComparator);
    }
}
