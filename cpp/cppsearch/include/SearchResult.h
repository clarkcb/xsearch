#ifndef CPPSEARCH_SEARCHRESULT_H
#define CPPSEARCH_SEARCHRESULT_H

#include <cstdlib>
#include "color.h"
#include "cppfind.h"

namespace cppsearch {
    class SearchResult {
    private:
        const cppfind::RegexPattern* m_pattern;
        cppfind::FileResult* m_file;
        unsigned long m_line_num;
        unsigned long m_match_start_idx;
        unsigned long m_match_end_idx;
        std::string m_line;
        std::vector<std::string> m_lines_before;
        std::vector<std::string> m_lines_after;
        void init(const cppfind::RegexPattern* pattern, cppfind::FileResult* file, unsigned long line_num,
                  unsigned long match_start_idx, unsigned long match_end_idx, const std::string& line,
                  std::vector<std::string>* lines_before, std::vector<std::string>* lines_after);

    public:
        SearchResult(const cppfind::RegexPattern* pattern, cppfind::FileResult* file,
                     unsigned long line_num, unsigned long match_start_idx, unsigned long match_end_idx,
                     const std::string& line);
        SearchResult(const cppfind::RegexPattern* pattern, cppfind::FileResult* file,
                     unsigned long line_num, unsigned long match_start_idx, unsigned long match_end_idx,
                     const std::string& line, std::vector<std::string>* lines_before,
                     std::vector<std::string>* lines_after);
        [[nodiscard]] const cppfind::RegexPattern* pattern() const;
        [[nodiscard]] const cppfind::FileResult* file() const;
        void set_file(cppfind::FileResult* file);
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
