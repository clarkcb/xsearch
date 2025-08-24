#include "SearchResultSorter.h"
#include "cppfind.h"

namespace cppsearch {
    SearchResultSorter::SearchResultSorter(const SearchSettings& settings) :  m_settings{settings} {
    }

    SearchResultSorter::SearchResultSorter(const std::unique_ptr<SearchSettings>& settings_ptr) : m_settings{*settings_ptr} {
    }

    SearchSettings SearchResultSorter::settings() const {
        return m_settings;
    }

    int cmp_by_search_fields(const SearchFileResult& sfr1, const SearchFileResult& sfr2) {
        if (sfr1.line_num() == sfr2.line_num()) {
            if (sfr1.match_start_idx() == sfr2.match_start_idx()) {
                return sfr1.match_end_idx() <= sfr2.match_end_idx() ? -1 : 1;
            }
            return sfr1.match_start_idx() < sfr2.match_start_idx() ? -1 : 1;
        }
        return sfr1.line_num() < sfr2.line_num() ? -1 : 1;
    }

    int cmp_search_file_results_by_path(const SearchFileResult& sfr1, const SearchFileResult& sfr2) {
        if (const int frcmp = cppfind::cmp_file_results_by_path(sfr1.file(), sfr2.file()); frcmp != 0) {
            return frcmp;
        }
        return cmp_by_search_fields(sfr1, sfr2);
    }

    int cmp_search_file_results_by_path_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2) {
        if (const int frcmp = cppfind::cmp_file_results_by_path_ci(sfr1.file(), sfr2.file()); frcmp != 0) {
            return frcmp;
        }
        return cmp_by_search_fields(sfr1, sfr2);
    }

    std::function<bool(SearchFileResult&, SearchFileResult&)> SearchResultSorter::get_cmp_search_file_results_by_path() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_path_ci(fr2, fr1) <= 0; };
            }
            return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_path(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_path_ci(fr1, fr2) <= 0; };
        }
        return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_path(fr1, fr2) <= 0; };
    }

    int cmp_search_file_results_by_name(const SearchFileResult& sfr1, const SearchFileResult& sfr2) {
        if (const int frcmp = cppfind::cmp_file_results_by_name(sfr1.file(), sfr2.file()); frcmp != 0) {
            return frcmp;
        }
        return cmp_by_search_fields(sfr1, sfr2);
    }

    int cmp_search_file_results_by_name_ci(const SearchFileResult& sfr1, const SearchFileResult& sfr2) {
        if (const int frcmp = cppfind::cmp_file_results_by_name_ci(sfr1.file(), sfr2.file()); frcmp != 0) {
            return frcmp;
        }
        return cmp_by_search_fields(sfr1, sfr2);
    }

    std::function<bool(SearchFileResult&, SearchFileResult&)> SearchResultSorter::get_cmp_search_file_results_by_name() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_name_ci(fr2, fr1) <= 0; };
            }
            return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_name(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_name_ci(fr1, fr2) <= 0; };
        }
        return [](const SearchFileResult& fr1, const SearchFileResult& fr2) { return cmp_search_file_results_by_name(fr1, fr2) <= 0; };
    }
}
