#ifndef CPPSEARCH_SEARCHER_H
#define CPPSEARCH_SEARCHER_H

#include "SearchResult.h"
#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {
    class Searcher {
    private:
        cppfind::Finder* m_finder;
        SearchSettings* m_settings;
        static void validate_settings(SearchSettings* settings);
        std::vector<SearchResult*> search_files(const std::vector<cppfind::FileResult*>& files);
        std::vector<SearchResult*> search_file(cppfind::FileResult* fr);

        std::vector<SearchResult*> search_text_file(cppfind::FileResult* fr);
        std::vector<SearchResult*> search_ifstream(std::ifstream& fin);
        std::vector<SearchResult*> search_ifstream_lines(std::ifstream& fin);
        std::vector<SearchResult*> search_ifstream_contents(std::ifstream& fin);
        std::vector<SearchResult*> search_multiline_string(std::string& s);
        std::vector<SearchResult*> search_binary_file(cppfind::FileResult* fr);
        std::vector<SearchResult*> search_archive_file(cppfind::FileResult* fr);

    public:
        explicit Searcher(SearchSettings* settings);
        std::vector<SearchResult*> search();
    };
}

#endif //CPPSEARCH_SEARCHER_H
