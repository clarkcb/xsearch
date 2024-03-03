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
        std::vector<SearchFileResult> search();

    private:
        cppfind::Finder m_finder;
        SearchSettings m_settings;
        static void validate_settings(const SearchSettings& settings);
        std::vector<SearchFileResult> search_files(const std::vector<cppfind::FileResult>& files);
        std::vector<SearchFileResult> search_file(const cppfind::FileResult& fr);

        std::vector<SearchFileResult> search_text_file(const cppfind::FileResult& fr);
        std::vector<SearchTextResult> search_ifstream(std::ifstream& fin);
        std::vector<SearchTextResult> search_ifstream_lines(std::ifstream& fin);
        std::vector<SearchTextResult> search_ifstream_contents(std::ifstream& fin);
        std::vector<SearchTextResult> search_multiline_string(const std::string& s);
        std::vector<SearchFileResult> search_binary_file(const cppfind::FileResult& fr);
        std::vector<SearchFileResult> search_archive_file(const cppfind::FileResult& fr);

    };
}

#endif //CPPSEARCH_SEARCHER_H
