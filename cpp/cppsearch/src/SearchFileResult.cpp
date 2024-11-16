#include "SearchFileResult.h"

namespace cppsearch {
    // SearchFileResult::SearchFileResult(const cppfind::FileResult& file, const cppfind::RegexPattern& pattern,
    //                                    const unsigned long line_num, const unsigned long match_start_idx,
    //                                    const unsigned long match_end_idx, const std::string& line) :
    // SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line),
    // m_file(file) {
    // }
    //
    // SearchFileResult::SearchFileResult(const cppfind::FileResult& file, const cppfind::RegexPattern& pattern,
    //                                    const unsigned long line_num, const unsigned long match_start_idx,
    //                                    const unsigned long match_end_idx, const std::string& line,
    //                                    const std::vector<std::string>& lines_before,
    //                                    const std::vector<std::string>& lines_after) :
    // SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line, lines_before, lines_after),
    // m_file(file) {
    // }

    SearchFileResult::SearchFileResult(const cppfind::FileResult& file, SearchTextResult&& text_result) :
    SearchTextResult(text_result),
    m_file(file)
    {
        // this->m_pattern = text_result.pattern();
        // this->m_line_num = text_result.line_num();
        // this->m_match_start_idx = text_result.match_start_idx();
        // this->m_match_end_idx = text_result.match_end_idx();
        // this->m_lines_before = text_result.lines_before();
        // this->m_lines_after = text_result.lines_after();
        // this->m_file = file;
    }

    cppfind::FileResult SearchFileResult::file() const {
        return m_file;
    }
}
