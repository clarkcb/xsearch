#include "SearchFileResult.h"

namespace cppsearch {
    SearchFileResult::SearchFileResult(const cppfind::FileResult& file, const cppfind::RegexPattern& pattern,
                                       const unsigned long line_num, const unsigned long match_start_idx,
                                       const unsigned long match_end_idx, const std::string& line) :
            SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line), m_file(file) {
    }

    SearchFileResult::SearchFileResult(const cppfind::FileResult& file, const cppfind::RegexPattern& pattern,
                                       const unsigned long line_num, const unsigned long match_start_idx,
                                       const unsigned long match_end_idx, const std::string& line,
                                       const std::vector<std::string>& lines_before,
                                       const std::vector<std::string>& lines_after) :
            SearchTextResult(pattern, line_num, match_start_idx, match_end_idx, line, lines_before, lines_after),
            m_file(file) {
    }

    SearchFileResult::SearchFileResult(const cppfind::FileResult& file, const SearchTextResult& text_result) :
            SearchTextResult(text_result), m_file(file) {
    }

    cppfind::FileResult SearchFileResult::file() const {
        return m_file;
    }
}
