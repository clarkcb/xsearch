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
        SearchFile* m_search_file;
        unsigned long m_line_num;
        unsigned long m_match_start_idx;
        unsigned long m_match_end_idx;
        std::string m_line;
        std::vector<std::string> m_lines_before;
        std::vector<std::string> m_lines_after;
        void init(const SearchPattern* pattern, SearchFile* search_file, unsigned long line_num,
                  unsigned long match_start_idx, unsigned long match_end_idx, const std::string& line,
                  std::vector<std::string>* lines_before, std::vector<std::string>* lines_after);

    public:
        SearchResult(const SearchPattern* pattern, SearchFile* search_file,
                     unsigned long line_num, unsigned long match_start_idx, unsigned long match_end_idx,
                     const std::string& line);
        SearchResult(const SearchPattern* pattern, SearchFile* search_file,
                     unsigned long line_num, unsigned long match_start_idx, unsigned long match_end_idx,
                     const std::string& line, std::vector<std::string>* lines_before,
                     std::vector<std::string>* lines_after);
        [[nodiscard]] const SearchPattern* pattern() const;
        [[nodiscard]] const SearchFile* search_file() const;
        void set_search_file(SearchFile* search_file);
        [[nodiscard]] unsigned long line_num() const;
        [[nodiscard]] unsigned long match_start_idx() const;
        [[nodiscard]] unsigned long match_end_idx() const;
        [[nodiscard]] std::string line() const;
        [[nodiscard]] std::vector<std::string> lines_before() const;
        [[nodiscard]] bool has_lines_before() const;
        [[nodiscard]] std::vector<std::string> lines_after() const;
        [[nodiscard]] bool has_lines_after() const;
    };
}

#endif //CPPSEARCH_SEARCHRESULT_H
