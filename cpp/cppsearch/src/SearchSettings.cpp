#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {

    SearchSettings::SearchSettings() :
    m_first_match{false},
    m_line_color{cppfind::Color::GREEN},
    m_lines_after{0},
    m_lines_before{0},
    m_max_line_length{150},
    m_multi_line_search{false},
    m_print_lines{false},
    m_print_results{false},
    m_search_archives{false},
    m_unique_lines{false} {
        // cppfind::print("SearchSettings()");
        archives_only(false);
    }

    // SearchSettings::~SearchSettings() {
    //     cppfind::print("~SearchSettings()");
    // }

    // bool SearchSettings::archives_only() const {
    //     return m_archives_only;
    // }
    //
    // void SearchSettings::archives_only(const bool archives_only) {
    //     m_archives_only = archives_only;
    //     if (archives_only) {
    //         m_include_archives = true;
    //         m_search_archives = true;
    //     }
    // }

    bool SearchSettings::first_match() const {
        return m_first_match;
    }

    void SearchSettings::first_match(const bool first_match) {
        m_first_match = first_match;
    }

    std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash> SearchSettings::in_lines_after_patterns() const {
        return m_in_lines_after_patterns;
    }

    std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash> SearchSettings::in_lines_before_patterns() const {
        return m_in_lines_before_patterns;
    }

    // bool SearchSettings::include_archives() const {
    //     return m_include_archives;
    // }
    //
    // void SearchSettings::include_archives(const bool include_archives) {
    //     m_include_archives = m_search_archives = include_archives;
    // }

    cppfind::Color SearchSettings::line_color() const {
        return m_line_color;
    }

    void SearchSettings::line_color(const cppfind::Color line_color) {
        m_line_color = line_color;
    }

    unsigned int SearchSettings::lines_after() const {
        return m_lines_after;
    }

    void SearchSettings::lines_after(const unsigned int line_count) {
        m_lines_after = line_count;
    }

    unsigned int SearchSettings::lines_before() const {
        return m_lines_before;
    }

    void SearchSettings::lines_before(const unsigned int line_count) {
        m_lines_before = line_count;
    }

    size_t SearchSettings::max_line_length() const {
        return m_max_line_length;
    }

    void SearchSettings::max_line_length(const size_t max) {
        m_max_line_length = max;
    }

    bool SearchSettings::multi_line_search() const {
        return m_multi_line_search;
    }

    void SearchSettings::multi_line_search(const bool multi_line_search) {
        m_multi_line_search = multi_line_search;
    }

    std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash> SearchSettings::out_lines_after_patterns() const {
        return m_out_lines_after_patterns;
    }

    std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash> SearchSettings::out_lines_before_patterns() const {
        return m_out_lines_before_patterns;
    }

    bool SearchSettings::print_lines() const {
        return m_print_lines;
    }

    void SearchSettings::print_lines(const bool print_lines) {
        m_print_lines = print_lines;
    }

    bool SearchSettings::print_matches() const {
        return m_print_matches;
    }

    void SearchSettings::print_matches(const bool print_matches) {
        m_print_matches = print_matches;
    }

    bool SearchSettings::print_results() const {
        return m_print_results;
    }

    void SearchSettings::print_results(const bool print_results) {
        m_print_results = print_results;
    }

    bool SearchSettings::search_archives() const {
        return m_search_archives;
    }

    void SearchSettings::search_archives(const bool search_archives) {
        m_search_archives = search_archives;
    }

    // void SearchSettings::search_archives_only(const bool search_archives_only) {
    //     FindSettings::archives_only(search_archives_only);
    //     if (search_archives_only) {
    //         m_search_archives = true;
    //     }
    // }

    std::unordered_set<cppfind::RegexPattern, cppfind::RegexPatternHash> SearchSettings::search_patterns() const {
        return m_search_patterns;
    }

    bool SearchSettings::unique_lines() const {
        return m_unique_lines;
    }

    void SearchSettings::unique_lines(const bool unique_lines) {
        m_unique_lines = unique_lines;
    }

    void SearchSettings::add_in_lines_after_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_in_lines_after_patterns);
    }

    void SearchSettings::add_in_lines_before_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_in_lines_before_patterns);
    }

    void SearchSettings::add_lines_after_to_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_lines_after_to_patterns);
    }

    void SearchSettings::add_lines_after_until_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_lines_after_until_patterns);
    }

    void SearchSettings::add_out_lines_after_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_out_lines_after_patterns);
    }

    void SearchSettings::add_out_lines_before_pattern(const std::string_view pattern) {
        add_pattern(pattern, m_out_lines_before_patterns);
    }

    void SearchSettings::add_search_pattern(const std::string_view pattern) {
        add_pattern(pattern, this->m_search_patterns);
    }

    std::string SearchSettings::string() const {
        return std::string("SearchSettings(")
                + "archives_only: " + cppfind::StringUtil::bool_to_string(archives_only())
                + ", colorize: " + cppfind::StringUtil::bool_to_string(colorize())
                + ", debug: " + cppfind::StringUtil::bool_to_string(debug())
                + ", first_match: " + cppfind::StringUtil::bool_to_string(m_first_match)
                + ", follow_symlinks: " + cppfind::StringUtil::bool_to_string(follow_symlinks())
                + ", in_archive_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(in_archive_extensions())
                + ", in_archive_file_patterns: " + patterns_to_string(in_archive_file_patterns())
                + ", in_dir_patterns: " + patterns_to_string(in_dir_patterns())
                + ", in_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(in_extensions())
                + ", in_file_patterns: " + patterns_to_string(in_file_patterns())
                + ", in_file_types: " + file_types_to_string(in_file_types())
                + ", in_lines_after_patterns: " + patterns_to_string(m_in_lines_after_patterns)
                + ", in_lines_before_patterns: " + patterns_to_string(m_in_lines_before_patterns)
                + ", include_archives: " + cppfind::StringUtil::bool_to_string(include_archives())
                + ", include_hidden: " + cppfind::StringUtil::bool_to_string(include_hidden())
                + ", lines_after: " + std::to_string(m_lines_after)
                + ", lines_after_to_patterns: " + patterns_to_string(m_lines_after_to_patterns)
                + ", lines_after_until_patterns: " + patterns_to_string(m_lines_after_until_patterns)
                + ", lines_before: " + std::to_string(m_lines_before)
                + ", max_depth: " + std::to_string(max_depth())
                + ", max_last_mod: \"" + cppfind::StringUtil::long_to_date_str(max_last_mod()) + "\""
                + ", max_line_length: " + std::to_string(m_max_line_length)
                + ", max_size: " + std::to_string(max_size())
                + ", min_depth: " + std::to_string(min_depth())
                + ", min_last_mod: \"" + cppfind::StringUtil::long_to_date_str(min_last_mod()) + "\""
                + ", min_size: " + std::to_string(min_size())
                + ", multi_line_search: " + cppfind::StringUtil::bool_to_string(m_multi_line_search)
                + ", out_archive_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(out_archive_extensions())
                + ", out_archive_file_patterns: " + patterns_to_string(out_archive_file_patterns())
                + ", out_dir_patterns: " + patterns_to_string(out_dir_patterns())
                + ", out_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(out_extensions())
                + ", out_file_patterns: " + patterns_to_string(out_file_patterns())
                + ", out_file_types: " + file_types_to_string(out_file_types())
                + ", out_lines_after_patterns: " + patterns_to_string(m_out_lines_after_patterns)
                + ", out_lines_before_patterns: " + patterns_to_string(m_out_lines_before_patterns)
                + ", paths: " + paths_to_string(paths())
                + ", print_dirs: " + cppfind::StringUtil::bool_to_string(print_dirs())
                + ", print_files: " + cppfind::StringUtil::bool_to_string(print_files())
                + ", print_lines: " + cppfind::StringUtil::bool_to_string(m_print_lines)
                + ", print_matches: " + cppfind::StringUtil::bool_to_string(m_print_matches)
                + ", print_results: " + cppfind::StringUtil::bool_to_string(m_print_results)
                + ", print_usage: " + cppfind::StringUtil::bool_to_string(print_usage())
                + ", print_version: " + cppfind::StringUtil::bool_to_string(print_version())
                + ", recursive: " + cppfind::StringUtil::bool_to_string(recursive())
                + ", search_archives: " + cppfind::StringUtil::bool_to_string(m_search_archives)
                + ", search_patterns: " + patterns_to_string(m_search_patterns)
                + ", sort_by: " + sort_by_to_name(sort_by())
                + ", sort_case_insensitive: " + cppfind::StringUtil::bool_to_string(sort_case_insensitive())
                + ", sort_descending: " + cppfind::StringUtil::bool_to_string(sort_descending())
                + ", unique_lines: " + cppfind::StringUtil::bool_to_string(m_unique_lines)
                + ", verbose: " + cppfind::StringUtil::bool_to_string(verbose())
                + ")";
    }

//    std::ostream& operator<<(std::ostream& strm, SearchSettings& settings) {
//        std::string settings_string = settings.string();
//        return strm << settings_string;
//    }
}
