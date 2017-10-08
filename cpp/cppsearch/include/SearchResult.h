#ifndef CPPSEARCH_SEARCHRESULT_H
#define CPPSEARCH_SEARCHRESULT_H

#include <cstdlib>
#include "SearchPattern.h"
#include "SearchFile.h"

using namespace std;

class SearchResult {
private:
    void init(const SearchPattern* pattern, SearchFile* searchfile,
              long linenum, long match_start_idx, long match_end_idx,
              string line, vector<string>* lines_before, vector<string>* lines_after);
    string single_line_result_to_string();
    string multi_line_result_to_string();
    long linenum_padding();

public:
    const SearchPattern* pattern;
    SearchFile* searchfile;
    long linenum;
    long match_start_idx;
    long match_end_idx;
    string line;
    vector<string> lines_before;
    vector<string> lines_after;
    SearchResult(const SearchPattern* pattern, SearchFile* searchfile,
                 long linenum, long match_start_idx, long match_end_idx,
                 string line);
    SearchResult(const SearchPattern* pattern, SearchFile* searchfile,
                 long linenum, long match_start_idx, long match_end_idx,
                 string line, vector<string>* lines_before, vector<string>* lines_after);
    string result_to_string();
};

#endif //CPPSEARCH_SEARCHRESULT_H
