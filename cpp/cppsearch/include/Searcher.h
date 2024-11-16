#ifndef CPPSEARCH_SEARCHER_H
#define CPPSEARCH_SEARCHER_H

//#include "SearchResult.h"
#include "SearchFileResult.h"
#include "SearchTextResult.h"
#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {
    class Searcher {
    public:
        explicit Searcher(const SearchSettings& settings);
        explicit Searcher(const std::unique_ptr<SearchSettings>& settings_ptr);
        Searcher(Searcher& other) = delete;
        Searcher(Searcher&& other) = delete;
        std::vector<SearchFileResult> search() const;

    private:
        cppfind::Finder m_finder;
        SearchSettings m_search_settings;
        static void validate_settings(const SearchSettings& settings);
        [[nodiscard]] std::vector<SearchFileResult> search_files(const std::vector<cppfind::FileResult>& files) const;
        [[nodiscard]] std::vector<SearchFileResult> search_file(const cppfind::FileResult& fr) const;

        [[nodiscard]] std::vector<SearchFileResult> search_text_file(const cppfind::FileResult& fr) const;
        std::vector<SearchTextResult> search_ifstream(std::ifstream& fin) const;
        std::vector<SearchTextResult> search_ifstream_lines(std::ifstream& fin) const;
        std::vector<SearchTextResult> search_ifstream_contents(std::ifstream& fin) const;
        [[nodiscard]] std::vector<SearchTextResult> search_multiline_string(const std::string& s) const;
        [[nodiscard]] std::vector<SearchFileResult> search_binary_file(const cppfind::FileResult& fr) const;
        // std::vector<SearchFileResult> search_archive_file(const cppfind::FileResult& fr);

    };
}

#endif //CPPSEARCH_SEARCHER_H
