#include "SearchResult.h"
#include "StringUtil.h"

namespace cppsearch {
    SearchResult::SearchResult(const SearchPattern* pattern, SearchFile* searchfile,
                               const unsigned long linenum, const unsigned long match_start_idx,
                               const unsigned long match_end_idx, const std::string& line) {
        std::vector<std::string> lines_before = {};
        std::vector<std::string> lines_after = {};
        init(pattern, searchfile, linenum, match_start_idx, match_end_idx, line, &lines_before,
             &lines_after);
    }

    SearchResult::SearchResult(const SearchPattern* pattern, SearchFile* searchfile,
                               const unsigned long linenum, const unsigned long match_start_idx,
                               const unsigned long match_end_idx, const std::string& line,
                               std::vector<std::string>* lines_before,
                               std::vector<std::string>* lines_after) {
        init(pattern, searchfile, linenum, match_start_idx, match_end_idx, line, lines_before,
             lines_after);
    }

    void SearchResult::init(const SearchPattern* pattern, SearchFile* searchfile,
                            const unsigned long linenum, const unsigned long match_start_idx,
                            const unsigned long match_end_idx, const std::string& line,
                            std::vector<std::string>* lines_before,
                            std::vector<std::string>* lines_after) {
        m_pattern = pattern;
        m_searchfile = searchfile;
        m_linenum = linenum;
        m_match_start_idx = match_start_idx;
        m_match_end_idx = match_end_idx;
        m_line = line;
        m_lines_before = *lines_before;
        m_lines_after = *lines_after;
    }

    const SearchPattern* SearchResult::pattern() const {
        return m_pattern;
    }

    const SearchFile* SearchResult::searchfile() const {
        return m_searchfile;
    }

    void SearchResult::set_searchfile(SearchFile* searchfile) {
        m_searchfile = searchfile;
    }

    unsigned long SearchResult::linenum() const {
        return m_linenum;
    }

    unsigned long SearchResult::match_start_idx() const {
        return m_match_start_idx;
    }

    unsigned long SearchResult::match_end_idx() const {
        return m_match_end_idx;
    }

    std::string SearchResult::line() const {
        return m_line;
    }

    std::vector<std::string> SearchResult::lines_before() const {
        return m_lines_before;
    }

    bool SearchResult::has_lines_before() const {
        return !m_lines_before.empty();
    }

    std::vector<std::string> SearchResult::lines_after() const {
        return m_lines_after;
    }

    bool SearchResult::has_lines_after() const {
        return !m_lines_after.empty();
    }
}
