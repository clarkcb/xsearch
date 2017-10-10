#ifndef CPPSEARCH_SEARCHER_H
#define CPPSEARCH_SEARCHER_H

#include "SearchFile.h"
#include "SearchResult.h"
#include "SearchSettings.h"

class Searcher {
private:
    FileTypes* filetypes;
    SearchSettings* settings;
    void validate_settings(SearchSettings* settings);
    vector<SearchResult*> search_path(string filepath);
    vector<SearchResult*> search_file(SearchFile* sf);
    bool is_search_dir(string filepath);
    vector<string> get_search_dirs(string filepath);
    bool is_search_file(string filepath);
    bool is_archive_search_file(string filepath);
    bool filter_file(string filepath);
    SearchFile* get_searchfile(string* filepath);
    vector<SearchFile*> get_search_files(vector<string> searchdirs);

    vector<SearchResult*> search_text_file(SearchFile* sf);
    vector<SearchResult*> search_ifstream(std::ifstream& fin);
    vector<SearchResult*> search_ifstream_lines(std::ifstream& fin);
    vector<SearchResult*> search_ifstream_contents(std::ifstream& fin);
    vector<SearchResult*> search_string(string* s);
    vector<SearchResult*> search_binary_file(SearchFile* sf);
    vector<SearchResult*> search_archive_file(SearchFile* sf);

public:
    explicit Searcher(SearchSettings* settings);
    vector<SearchResult*> search();
};

#endif //CPPSEARCH_SEARCHER_H
