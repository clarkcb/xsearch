using System;
using System.Collections.Generic;
using CsFindLib;

namespace CsSearchLib;

public class SearchResultSorter(SearchSettings settings)
{
    private SearchSettings Settings { get; } =  settings;

    private Comparison<SearchResult> GetSearchResultsComparison()
    {
        if (Settings.SortDescending)
        {
            return Settings.SortBy switch
            {
                SortBy.FileName => (r1, r2) => r2.CompareByName(r1, Settings.SortCaseInsensitive),
                SortBy.FileSize => (r1, r2) => r2.CompareBySize(r1, Settings.SortCaseInsensitive),
                SortBy.FileType => (r1, r2) => r2.CompareByType(r1, Settings.SortCaseInsensitive),
                SortBy.LastMod => (r1, r2) => r2.CompareByLastMod(r1, Settings.SortCaseInsensitive),
                _ => (r1, r2) => r2.CompareByPath(r1, Settings.SortCaseInsensitive)
            };
        }

        return Settings.SortBy switch
        {
            SortBy.FileName => (r1, r2) => r1.CompareByName(r2, Settings.SortCaseInsensitive),
            SortBy.FileSize => (r1, r2) => r1.CompareBySize(r2, Settings.SortCaseInsensitive),
            SortBy.FileType => (r1, r2) => r1.CompareByType(r2, Settings.SortCaseInsensitive),
            SortBy.LastMod => (r1, r2) => r1.CompareByLastMod(r2, Settings.SortCaseInsensitive),
            _ => (r1, r2) => r1.CompareByPath(r2, Settings.SortCaseInsensitive)
        };
    }

    public void Sort(List<SearchResult> results)
    {
        var comparison = GetSearchResultsComparison();
        results.Sort(comparison);

        if (Settings.SortDescending)
        {
            results.Reverse();
        }
    }
}