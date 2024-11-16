#include "SearchTextResult.h"

namespace cppsearch {
    SearchTextResult::SearchTextResult(const cppfind::RegexPattern& pattern,
                                       const unsigned long line_num, const unsigned long match_start_idx,
                                       const unsigned long match_end_idx, std::string line,
                                       const std::vector<std::string>& lines_before,
                                       const std::vector<std::string>& lines_after) :
    m_pattern(pattern),
    m_line_num(line_num),
    m_match_start_idx(match_start_idx),
    m_match_end_idx(match_end_idx),
    m_line(std::move(line)),
    m_lines_before(lines_before),
    m_lines_after(lines_after) {
    }

    SearchTextResult::SearchTextResult(const cppfind::RegexPattern& pattern,
                                       const unsigned long line_num, const unsigned long match_start_idx,
                                       const unsigned long match_end_idx, std::string line) :
    m_pattern(pattern),
    m_line_num(line_num),
    m_match_start_idx(match_start_idx),
    m_match_end_idx(match_end_idx),
    m_line(std::move(line)) {
    }

    // SearchTextResult::SearchTextResult(const SearchTextResult& other) : m_pattern(other.pattern()),
    //     m_line_num(other.line_num()), m_match_start_idx(other.match_start_idx()),
    //     m_match_end_idx(other.match_end_idx()), m_line(other.line()), m_lines_before(other.lines_before()),
    //     m_lines_after(other.lines_after()) {
    // }

//    SearchTextResult::SearchTextResult(SearchTextResult&& other) noexcept : m_pattern(other.pattern()),
//        m_line_num(other.line_num()), m_match_start_idx(other.match_start_idx()),
//        m_match_end_idx(other.match_end_idx()), m_line(other.line()), m_lines_before(other.lines_before()),
//        m_lines_after(other.lines_after()) {
//    }

    // void SearchTextResult::init(const cppfind::RegexPattern& pattern,
    //                             const unsigned long line_num, const unsigned long match_start_idx,
    //                             const unsigned long match_end_idx, const std::string& line,
    //                             const std::vector<std::string>& lines_before,
    //                             const std::vector<std::string>& lines_after) {
    //     m_pattern = pattern;
    //     m_line_num = line_num;
    //     m_match_start_idx = match_start_idx;
    //     m_match_end_idx = match_end_idx;
    //     m_line = line;
    //     m_lines_before = lines_before;
    //     m_lines_after = lines_after;
    // }

    cppfind::RegexPattern SearchTextResult::pattern() const {
        return m_pattern;
    }

    unsigned long SearchTextResult::line_num() const {
        return m_line_num;
    }

    unsigned long SearchTextResult::match_start_idx() const {
        return m_match_start_idx;
    }

    unsigned long SearchTextResult::match_end_idx() const {
        return m_match_end_idx;
    }

    std::string SearchTextResult::line() const {
        return m_line;
    }

    std::vector<std::string> SearchTextResult::lines_before() const {
        return m_lines_before;
    }

    bool SearchTextResult::has_lines_before() const {
        return !m_lines_before.empty();
    }

    std::vector<std::string> SearchTextResult::lines_after() const {
        return m_lines_after;
    }

    bool SearchTextResult::has_lines_after() const {
        return !m_lines_after.empty();
    }
}
