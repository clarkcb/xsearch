#ifndef CPPSEARCH_SEARCHRESULT_H
#define CPPSEARCH_SEARCHRESULT_H

#include <cstdlib>
#include "color.h"
#include "SearchPattern.h"
#include "SearchFile.h"

namespace cppsearch {
    class SearchResult {
    private:
        const SearchPattern* m_pattern;
        SearchFile* m_searchfile;
        unsigned long m_linenum;
        unsigned long m_match_start_idx;
        unsigned long m_match_end_idx;
        std::string m_line;
        std::vector<std::string> m_lines_before;
        std::vector<std::string> m_lines_after;
        void init(const SearchPattern* pattern, SearchFile* searchfile, unsigned long linenum,
                  unsigned long match_start_idx, unsigned long match_end_idx, const std::string& line,
                  std::vector<std::string>* lines_before, std::vector<std::string>* lines_after);

    public:
        SearchResult(const SearchPattern* pattern, SearchFile* searchfile,
                     unsigned long linenum, unsigned long match_start_idx, unsigned long match_end_idx,
                     const std::string& line);
        SearchResult(const SearchPattern* pattern, SearchFile* searchfile,
                     unsigned long linenum, unsigned long match_start_idx, unsigned long match_end_idx,
                     const std::string& line, std::vector<std::string>* lines_before,
                     std::vector<std::string>* lines_after);
        const SearchPattern* pattern() const;
        const SearchFile* searchfile() const;
        void set_searchfile(SearchFile* searchfile);
        unsigned long linenum() const;
        unsigned long match_start_idx() const;
        unsigned long match_end_idx() const;
        std::string line() const;
        std::vector<std::string> lines_before() const;
        bool has_lines_before() const;
        std::vector<std::string> lines_after() const;
        bool has_lines_after() const;
    };
}

#endif //CPPSEARCH_SEARCHRESULT_H
