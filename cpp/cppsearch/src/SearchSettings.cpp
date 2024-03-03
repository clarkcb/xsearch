#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {

    SearchSettings::SearchSettings() : cppfind::FindSettings() {
    }

    bool SearchSettings::colorize() const {
        return m_colorize;
    }

    void SearchSettings::colorize(const bool colorize) {
        m_colorize = colorize;
    }

    bool SearchSettings::first_match() const {
        return m_first_match;
    }

    void SearchSettings::first_match(const bool first_match) {
        m_first_match = first_match;
    }

    std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> SearchSettings::in_lines_after_patterns() const {
        return m_in_lines_after_patterns;
    }

    std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> SearchSettings::in_lines_before_patterns() const {
        return m_in_lines_before_patterns;
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

    std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> SearchSettings::out_lines_after_patterns() const {
        return m_out_lines_after_patterns;
    }

    std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> SearchSettings::out_lines_before_patterns() const {
        return m_out_lines_before_patterns;
    }

    bool SearchSettings::print_lines() const {
        return m_print_lines;
    }

    void SearchSettings::print_lines(const bool print_lines) {
        m_print_lines = print_lines;
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

    void SearchSettings::search_archives_only(const bool search_archives_only) {
        FindSettings::archives_only(search_archives_only);
        if (search_archives_only) {
            m_search_archives = true;
        }
    }

    std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> SearchSettings::search_patterns() const {
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
        add_pattern(pattern, m_search_patterns);
    }

    std::string SearchSettings::string() {
        const auto in_archive_exts = in_archive_extensions();
        const auto in_exts = in_extensions();
        const auto out_archive_exts = out_archive_extensions();
        const auto out_exts = out_extensions();
        const auto pths = paths();
        auto settings_str =
                std::string("SearchSettings(")
                + "archives_only: " + cppfind::StringUtil::bool_to_string(archives_only())
                + ", colorize: " + cppfind::StringUtil::bool_to_string(m_colorize)
                + ", debug: " + cppfind::StringUtil::bool_to_string(debug())
                + ", first_match: " + cppfind::StringUtil::bool_to_string(m_first_match)
                + ", in_archive_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(in_archive_exts)
                + ", in_archive_file_patterns: " + patterns_to_string(in_archive_file_patterns())
                + ", in_dir_patterns: " + patterns_to_string(in_dir_patterns())
                + ", in_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(in_exts)
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
                + ", list_dirs: " + cppfind::StringUtil::bool_to_string(print_dirs())
                + ", list_files: " + cppfind::StringUtil::bool_to_string(print_files())
                + ", list_lines: " + cppfind::StringUtil::bool_to_string(m_print_lines)
                + ", max_depth: " + std::to_string(max_depth())
                + ", max_last_mod: \"" + cppfind::StringUtil::long_to_date_str(max_last_mod()) + "\""
                + ", max_line_length: " + std::to_string(m_max_line_length)
                + ", max_size: " + std::to_string(max_size())
                + ", min_depth: " + std::to_string(min_depth())
                + ", min_last_mod: \"" + cppfind::StringUtil::long_to_date_str(min_last_mod()) + "\""
                + ", min_size: " + std::to_string(min_size())
                + ", multi_line_search: " + cppfind::StringUtil::bool_to_string(m_multi_line_search)
                + ", out_archive_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(out_archive_exts)
                + ", out_archive_file_patterns: " + patterns_to_string(out_archive_file_patterns())
                + ", out_dir_patterns: " + patterns_to_string(out_dir_patterns())
                + ", out_extensions: " + cppfind::StringUtil::unordered_string_set_to_string(out_exts)
                + ", out_file_patterns: " + patterns_to_string(out_file_patterns())
                + ", out_file_types: " + file_types_to_string(out_file_types())
                + ", out_lines_after_patterns: " + patterns_to_string(m_out_lines_after_patterns)
                + ", out_lines_before_patterns: " + patterns_to_string(m_out_lines_before_patterns)
                + ", paths: " + cppfind::StringUtil::unordered_string_set_to_string(pths)
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
        return settings_str;
    }

//    std::ostream& operator<<(std::ostream& strm, SearchSettings& settings) {
//        std::string settings_string = settings.string();
//        return strm << settings_string;
//    }
}
