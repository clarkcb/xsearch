#ifndef CPPSEARCH_SEARCHER_H
#define CPPSEARCH_SEARCHER_H

#include "SearchFile.h"
#include "SearchResult.h"
#include "SearchSettings.h"

namespace cppsearch {
    class Searcher {
    private:
        FileTypes* m_file_types;
        SearchSettings* m_settings;
        static void validate_settings(SearchSettings* settings);
        std::vector<SearchResult*> search_path(const std::string& file_path);
        std::vector<SearchResult*> search_file(SearchFile* sf);
        SearchFile* get_search_file(std::string& file_path);
        std::vector<SearchFile*> get_search_files(const std::string& file_path);

        std::vector<SearchResult*> search_text_file(SearchFile* sf);
        std::vector<SearchResult*> search_ifstream(std::ifstream& fin);
        std::vector<SearchResult*> search_ifstream_lines(std::ifstream& fin);
        std::vector<SearchResult*> search_ifstream_contents(std::ifstream& fin);
        std::vector<SearchResult*> search_multiline_string(std::string& s);
        std::vector<SearchResult*> search_binary_file(SearchFile* sf);
        std::vector<SearchResult*> search_archive_file(SearchFile* sf);

    public:
        explicit Searcher(SearchSettings* settings);
        bool filter_file(const std::string& file_path);
        bool is_archive_search_file(const std::string& file_path);
        bool is_search_dir(const std::string& file_path);
        bool is_search_file(const std::string& file_path);
        std::vector<SearchResult*> search();
    };
}

#endif //CPPSEARCH_SEARCHER_H
