#ifndef CPPSEARCH_SEARCHTEXTRESULT_H
#define CPPSEARCH_SEARCHTEXTRESULT_H

#include "cppfind.h"

namespace cppsearch {
    class SearchTextResult {
    public:
        SearchTextResult(const cppfind::RegexPattern& pattern, unsigned long line_num,
                         unsigned long match_start_idx, unsigned long match_end_idx,
                         std::string line, const std::vector<std::string>& lines_before,
                         const std::vector<std::string>& lines_after);
        SearchTextResult(const cppfind::RegexPattern& pattern, unsigned long line_num,
                         unsigned long match_start_idx, unsigned long match_end_idx,
                         std::string line);
        SearchTextResult(const SearchTextResult& other) = default;
        //SearchTextResult(SearchTextResult&& other) noexcept = default;
        [[nodiscard]] cppfind::RegexPattern pattern() const;
        [[nodiscard]] unsigned long line_num() const;
        [[nodiscard]] unsigned long match_start_idx() const;
        [[nodiscard]] unsigned long match_end_idx() const;
        [[nodiscard]] std::string line() const;
        [[nodiscard]] std::vector<std::string> lines_before() const;
        [[nodiscard]] bool has_lines_before() const;
        [[nodiscard]] std::vector<std::string> lines_after() const;
        [[nodiscard]] bool has_lines_after() const;

    protected:
        cppfind::RegexPattern m_pattern;
        unsigned long m_line_num;
        unsigned long m_match_start_idx;
        unsigned long m_match_end_idx;
        std::string m_line;
        std::vector<std::string> m_lines_before;
        std::vector<std::string> m_lines_after;

    // private:
    //     void init(const cppfind::RegexPattern& pattern, unsigned long line_num,
    //               unsigned long match_start_idx, unsigned long match_end_idx, const std::string& line,
    //               const std::vector<std::string>& lines_before, const std::vector<std::string>& lines_after);

    };
}

#endif //CPPSEARCH_SEARCHTEXTRESULT_H
