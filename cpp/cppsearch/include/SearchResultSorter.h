#ifndef CPPSEARCH_SEARCHRESULTSORTER_H
#define CPPSEARCH_SEARCHRESULTSORTER_H

#include "SearchFileResult.h"
#include "SearchSettings.h"

namespace cppsearch {
    class SearchResultSorter {
    public:
        explicit SearchResultSorter(const SearchSettings& settings);
        explicit SearchResultSorter(const std::unique_ptr<SearchSettings>& settings_ptr);
        SearchResultSorter(SearchResultSorter& other) = delete;
        SearchResultSorter(SearchResultSorter&& other) = delete;
        [[nodiscard]] SearchSettings settings() const;
        void sort(std::vector<SearchFileResult>& search_file_results) const;

    private:
        const SearchSettings m_settings;
        [[nodiscard]] std::function<bool(SearchFileResult&, SearchFileResult&)> get_cmp_search_file_results_by_path() const;
        [[nodiscard]] std::function<bool(SearchFileResult&, SearchFileResult&)> get_cmp_search_file_results_by_name() const;
        [[nodiscard]] std::function<bool(SearchFileResult&, SearchFileResult&)> get_cmp_search_file_results_by_size() const;
        [[nodiscard]] std::function<bool(SearchFileResult&, SearchFileResult&)> get_cmp_search_file_results_by_type() const;
        [[nodiscard]] std::function<bool(SearchFileResult&, SearchFileResult&)> get_cmp_search_file_results_by_lastmod() const;
        [[nodiscard]] std::function<bool(SearchFileResult&, SearchFileResult&)> get_search_file_result_comparator() const;
    };

    int cmp_search_file_results_by_path(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_path_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_name(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_name_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_size(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_size_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_type(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_type_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_lastmod(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
    int cmp_search_file_results_by_lastmod_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2);
}

#endif //CPPSEARCH_SEARCHRESULTSORTER_H
