#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {
    SearchSettings::SearchSettings() : cppfind::FindSettings() {
        m_in_lines_after_patterns = {};
        m_in_lines_before_patterns = {};
        m_lines_after_to_patterns = {};
        m_lines_after_until_patterns = {};
        m_out_lines_after_patterns = {};
        m_out_lines_before_patterns = {};
        m_search_patterns = {};
    }

    void SearchSettings::add_in_lines_after_pattern(const std::string& p) {
        add_pattern(p, &m_in_lines_after_patterns);
    }

    void SearchSettings::add_in_lines_before_pattern(const std::string& p) {
        add_pattern(p, &m_in_lines_before_patterns);
    }

    void SearchSettings::add_lines_after_to_pattern(const std::string& p) {
        add_pattern(p, &m_lines_after_to_patterns);
    }

    void SearchSettings::add_lines_after_until_pattern(const std::string& p) {
        add_pattern(p, &m_lines_after_until_patterns);
    }

    void SearchSettings::add_out_lines_after_pattern(const std::string& p) {
        add_pattern(p, &m_out_lines_after_patterns);
    }

    void SearchSettings::add_out_lines_before_pattern(const std::string& p) {
        add_pattern(p, &m_out_lines_before_patterns);
    }

    void SearchSettings::add_search_pattern(const std::string& p) {
        add_pattern(p, &m_search_patterns);
    }

    bool SearchSettings::archives_only() const {
        return m_archives_only;
    }

    bool SearchSettings::colorize() const {
        return m_colorize;
    }

    bool SearchSettings::first_match() const {
        return m_first_match;
    }

    bool SearchSettings::multi_line_search() const {
        return m_multi_line_search;
    }

    unsigned int SearchSettings::lines_after() const {
        return m_lines_after;
    }

    unsigned int SearchSettings::lines_before() const {
        return m_lines_before;
    }

    bool SearchSettings::list_lines() const {
        return m_list_lines;
    }

    size_t SearchSettings::max_line_length() const {
        return m_max_line_length;
    }

    bool SearchSettings::print_results() const {
        return m_print_results;
    }

    bool SearchSettings::search_archives() const {
        return m_search_archives;
    }

    std::vector<cppfind::RegexPattern*>* SearchSettings::in_lines_after_patterns() {
        return &m_in_lines_after_patterns;
    }

    std::vector<cppfind::RegexPattern*>* SearchSettings::in_lines_before_patterns() {
        return &m_in_lines_before_patterns;
    }

    std::vector<cppfind::RegexPattern*>* SearchSettings::out_lines_after_patterns() {
        return &m_out_lines_after_patterns;
    }

    std::vector<cppfind::RegexPattern*>* SearchSettings::out_lines_before_patterns() {
        return &m_out_lines_before_patterns;
    }

    std::vector<cppfind::RegexPattern*>* SearchSettings::search_patterns() {
        return &m_search_patterns;
    }

    bool SearchSettings::unique_lines() const {
        return m_unique_lines;
    }

    void SearchSettings::archives_only(const bool b) {
        m_archives_only = b;
        if (b) {
            m_include_archives = b;
            m_search_archives = b;
        }
    }

    void SearchSettings::colorize(const bool b) {
        m_colorize = b;
    }

    void SearchSettings::first_match(const bool b) {
        m_first_match = b;
    }

    void SearchSettings::lines_after(const unsigned int line_count) {
        m_lines_after = line_count;
    }

    void SearchSettings::lines_before(const unsigned int line_count) {
        m_lines_before = line_count;
    }

    void SearchSettings::list_lines(const bool b) {
        m_list_lines = b;
    }

    void SearchSettings::max_line_length(const size_t max) {
        m_max_line_length = max;
    }

    void SearchSettings::multi_line_search(const bool b) {
        m_multi_line_search = b;
    }

    void SearchSettings::print_results(const bool b) {
        m_print_results = b;
    }

    void SearchSettings::search_archives(const bool b) {
        m_search_archives = b;
    }

    void SearchSettings::unique_lines(const bool b) {
        m_unique_lines = b;
    }

    std::string SearchSettings::search_patterns_to_string(std::vector<cppfind::RegexPattern*>* ps) {
        std::string ps_string = "[";
        int count = 0;
        for (auto const& p : *ps) {
            if (count > 0) {
                ps_string.append(", ");
            }
            ps_string.append("\"").append(p->pattern()).append("\"");
            count++;
        }
        ps_string.append("]");
        return ps_string;
    }

    std::string SearchSettings::string() {
        auto settings_str =
                std::string("SearchSettings(")
                + "archives_only: " + bool_to_string(archives_only())
                + ", colorize: " + bool_to_string(m_colorize)
                + ", debug: " + bool_to_string(debug())
                + ", exclude_hidden: " + bool_to_string(exclude_hidden())
                + ", first_match: " + bool_to_string(m_first_match)
                + ", in_archive_extensions: " + string_vector_to_string(in_archive_extensions())
                + ", in_archive_file_patterns: " + search_patterns_to_string(in_archive_file_patterns())
                + ", in_dir_patterns: " + search_patterns_to_string(in_dir_patterns())
                + ", in_extensions: " + string_vector_to_string(in_extensions())
                + ", in_file_patterns: " + search_patterns_to_string(in_file_patterns())
                + ", in_file_types: " + file_types_to_string(in_file_types())
                + ", in_lines_after_patterns: " + search_patterns_to_string(&m_in_lines_after_patterns)
                + ", in_lines_before_patterns: " + search_patterns_to_string(&m_in_lines_before_patterns)
                + ", include_archives: " + bool_to_string(m_include_archives)
                + ", lines_after: " + std::to_string(m_lines_after)
                + ", lines_after_to_patterns: " + search_patterns_to_string(&m_lines_after_to_patterns)
                + ", lines_after_until_patterns: " + search_patterns_to_string(&m_lines_after_until_patterns)
                + ", lines_before: " + std::to_string(m_lines_before)
                + ", list_dirs: " + bool_to_string(list_dirs())
                + ", list_files: " + bool_to_string(list_files())
                + ", list_lines: " + bool_to_string(m_list_lines)
                + ", max_depth: " + std::to_string(m_max_depth)
                + ", max_last_mod: \"" + cppfind::long_to_datestr(m_max_last_mod) + "\""
                + ", max_line_length: " + std::to_string(m_max_line_length)
                + ", max_size: " + std::to_string(m_max_size)
                + ", min_depth: " + std::to_string(m_min_depth)
                + ", min_last_mod: \"" + cppfind::long_to_datestr(m_min_last_mod) + "\""
                + ", min_size: " + std::to_string(m_min_size)
                + ", multi_line_search: " + bool_to_string(m_multi_line_search)
                + ", out_archive_extensions: " + string_vector_to_string(out_archive_extensions())
                + ", out_archive_file_patterns: " + search_patterns_to_string(out_archive_file_patterns())
                + ", out_dir_patterns: " + search_patterns_to_string(out_dir_patterns())
                + ", out_extensions: " + string_vector_to_string(out_extensions())
                + ", out_file_patterns: " + search_patterns_to_string(out_file_patterns())
                + ", out_file_types: " + file_types_to_string(out_file_types())
                + ", out_lines_after_patterns: " + search_patterns_to_string(&m_out_lines_after_patterns)
                + ", out_lines_before_patterns: " + search_patterns_to_string(&m_out_lines_before_patterns)
                + ", paths: " + string_vector_to_string(paths())
                + ", print_results: " + bool_to_string(m_print_results)
                + ", print_usage: " + bool_to_string(print_usage())
                + ", print_version: " + bool_to_string(print_version())
                + ", recursive: " + bool_to_string(recursive())
                + ", search_archives: " + bool_to_string(m_search_archives)
                + ", search_patterns: " + search_patterns_to_string(&m_search_patterns)
                + ", sort_by: " + sort_by_to_name(m_sort_by)
                + ", sort_case_insensitive: " + bool_to_string(m_sort_case_insensitive)
                + ", sort_descending: " + bool_to_string(m_sort_descending)
                + ", unique_lines: " + bool_to_string(m_unique_lines)
                + ", verbose: " + bool_to_string(verbose())
                + ")";
        return settings_str;
    }

//    std::ostream& operator<<(std::ostream& strm, SearchSettings& settings) {
//        std::string settings_string = settings.string();
//        return strm << settings_string;
//    }
}
