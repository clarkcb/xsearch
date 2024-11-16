#ifndef CPPSEARCH_SEARCHRESULT_H
#define CPPSEARCH_SEARCHRESULT_H

#include "cppfind.h"
#include "SearchTextResult.h"

namespace cppsearch {
    class SearchFileResult : public SearchTextResult {
    public:
        // SearchFileResult(const cppfind::FileResult& file, const cppfind::RegexPattern& pattern,
        //                  unsigned long line_num, unsigned long match_start_idx, unsigned long match_end_idx,
        //                  const std::string& line);
        // SearchFileResult(const cppfind::FileResult& file, const cppfind::RegexPattern& pattern,
        //                  unsigned long line_num, unsigned long match_start_idx, unsigned long match_end_idx,
        //                  const std::string& line, const std::vector<std::string>& lines_before,
        //                  const std::vector<std::string>& lines_after);
        explicit SearchFileResult(const cppfind::FileResult& file, SearchTextResult&& text_result);
        SearchFileResult(const SearchFileResult& other) = default;
        //SearchFileResult(SearchFileResult&& other) = default;
        [[nodiscard]] cppfind::FileResult file() const;

    private:
        cppfind::FileResult m_file;
    };
}

#endif //CPPSEARCH_SEARCHRESULT_H
