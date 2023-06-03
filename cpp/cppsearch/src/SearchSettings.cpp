#include "FileUtil.h"
#include "StringUtil.h"
#include "SearchSettings.h"

namespace cppsearch {
    SearchSettings::SearchSettings() {
        m_in_archive_extensions = {};
        m_in_archive_file_patterns = {};
        m_in_dir_patterns = {};
        m_in_extensions = {};
        m_in_file_patterns = {};
        m_in_file_types = {};
        m_out_archive_extensions = {};
        m_out_archive_file_patterns = {};
        m_out_dir_patterns = {};
        m_out_extensions = {};
        m_out_file_patterns = {};
        m_out_file_types = {};
        m_paths = {};
        m_search_patterns = {};
    }

    void SearchSettings::add_pattern(const std::string& p, std::vector<SearchPattern*>* ps) {
        ps->push_back(new SearchPattern(p));
    }

    void SearchSettings::add_extensions(const std::string& exts, std::vector<std::string>* extensions) {
        std::vector<std::string> xs = StringUtil::split_string(exts, ",");
        for (const auto& x : xs) {
            if (!x.empty()) {
                extensions->push_back(x);
            }
        }
    }

    void SearchSettings::add_in_archive_extension(const std::string& ext) {
        add_extensions(ext, &m_in_archive_extensions);
    }

    void SearchSettings::add_in_archive_file_pattern(const std::string& p) {
        add_pattern(p, &m_in_archive_file_patterns);
    }

    void SearchSettings::add_in_dir_pattern(const std::string& p) {
        add_pattern(p, &m_in_dir_patterns);
    }

    void SearchSettings::add_in_extension(const std::string& ext) {
        add_extensions(ext, &m_in_extensions);
    }

    void SearchSettings::add_in_file_pattern(const std::string& p) {
        add_pattern(p, &m_in_file_patterns);
    }

    void SearchSettings::add_in_file_type(const FileType file_type) {
        m_in_file_types.push_back(file_type);
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

    void SearchSettings::add_out_archive_extension(const std::string& ext) {
        add_extensions(ext, &m_out_archive_extensions);
    }

    void SearchSettings::add_out_archive_file_pattern(const std::string& p) {
        add_pattern(p, &m_out_archive_file_patterns);
    }

    void SearchSettings::add_out_dir_pattern(const std::string& p) {
        add_pattern(p, &m_out_dir_patterns);
    }

    void SearchSettings::add_out_extension(const std::string& ext) {
        add_extensions(ext, &m_out_extensions);
    }

    void SearchSettings::add_out_file_pattern(const std::string& p) {
        add_pattern(p, &m_out_file_patterns);
    }

    void SearchSettings::add_out_file_type(const FileType file_type) {
        m_out_file_types.push_back(file_type);
    }

    void SearchSettings::add_out_lines_after_pattern(const std::string& p) {
        add_pattern(p, &m_out_lines_after_patterns);
    }

    void SearchSettings::add_out_lines_before_pattern(const std::string& p) {
        add_pattern(p, &m_out_lines_before_patterns);
    }

    void SearchSettings::add_path(const std::string& p) {
        m_paths.push_back(p);
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

    bool SearchSettings::debug() const {
        return m_debug;
    }

    bool SearchSettings::exclude_hidden() const {
        return m_exclude_hidden;
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

    bool SearchSettings::list_dirs() const {
        return m_list_dirs;
    }

    bool SearchSettings::list_files() const {
        return m_list_files;
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

    bool SearchSettings::print_usage() const {
        return m_print_usage;
    }

    bool SearchSettings::print_version() const {
        return m_print_version;
    }

    bool SearchSettings::recursive() const {
        return m_recursive;
    }

    bool SearchSettings::search_archives() const {
        return m_search_archives;
    }

    std::vector<std::string>* SearchSettings::in_archive_extensions() {
        return &m_in_archive_extensions;
    }

    std::vector<SearchPattern*>* SearchSettings::in_archive_file_patterns() {
        return &m_in_archive_file_patterns;
    }

    std::vector<SearchPattern*>* SearchSettings::in_dir_patterns() {
        return &m_in_dir_patterns;
    }

    std::vector<std::string>* SearchSettings::in_extensions() {
        return &m_in_extensions;
    }

    std::vector<SearchPattern*>* SearchSettings::in_file_patterns() {
        return &m_in_file_patterns;
    }

    std::vector<SearchPattern*>* SearchSettings::in_lines_after_patterns() {
        return &m_in_lines_after_patterns;
    }

    std::vector<SearchPattern*>* SearchSettings::in_lines_before_patterns() {
        return &m_in_lines_before_patterns;
    }

    std::vector<std::string>* SearchSettings::out_archive_extensions() {
        return &m_out_archive_extensions;
    }

    std::vector<SearchPattern*>* SearchSettings::out_archive_file_patterns() {
        return &m_out_archive_file_patterns;
    }

    std::vector<SearchPattern*>* SearchSettings::out_dir_patterns() {
        return &m_out_dir_patterns;
    }

    std::vector<std::string>* SearchSettings::out_extensions() {
        return &m_out_extensions;
    }

    std::vector<SearchPattern*>* SearchSettings::out_file_patterns() {
        return &m_out_file_patterns;
    }

    std::vector<SearchPattern*>* SearchSettings::out_lines_after_patterns() {
        return &m_out_lines_after_patterns;
    }

    std::vector<SearchPattern*>* SearchSettings::out_lines_before_patterns() {
        return &m_out_lines_before_patterns;
    }

    std::vector<std::string>* SearchSettings::paths() {
        return &m_paths;
    }

    std::vector<SearchPattern*>* SearchSettings::search_patterns() {
        return &m_search_patterns;
    }

    bool SearchSettings::unique_lines() const {
        return m_unique_lines;
    }

    bool SearchSettings::verbose() const {
        return m_verbose;
    }

    void SearchSettings::archives_only(const bool b) {
        m_archives_only = b;
        if (b) m_search_archives = b;
    }

    void SearchSettings::colorize(const bool b) {
        m_colorize = b;
    }

    void SearchSettings::debug(const bool b) {
        m_debug = b;
        if (b) m_verbose = b;
    }

    void SearchSettings::exclude_hidden(const bool b) {
        m_exclude_hidden = b;
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

    void SearchSettings::list_dirs(const bool b) {
        m_list_dirs = b;
    }

    void SearchSettings::list_files(const bool b) {
        m_list_files = b;
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

    void SearchSettings::print_usage(const bool b) {
        m_print_usage = b;
    }

    void SearchSettings::print_version(const bool b) {
        m_print_version = b;
    }

    void SearchSettings::recursive(const bool b) {
        m_recursive = b;
    }

    void SearchSettings::search_archives(const bool b) {
        m_search_archives = b;
    }

    void SearchSettings::unique_lines(const bool b) {
        m_unique_lines = b;
    }

    void SearchSettings::verbose(const bool b) {
        m_verbose = b;
    }

    std::string SearchSettings::bool_to_string(bool b) {
        return b ? "true" : "false";
    }

    std::string SearchSettings::string_vector_to_string(std::vector<std::string>* ss) {
        std::string ss_string = "[";
        int count = 0;
        for (auto const& s : *ss) {
            if (count > 0) {
                ss_string.append(", ");
            }
            ss_string.append("\"").append(s).append("\"");
            count++;
        }
        ss_string.append("]");
        return ss_string;
    }

    std::string SearchSettings::search_patterns_to_string(std::vector<SearchPattern*>* ps) {
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
                + "archives_only: " + bool_to_string(m_archives_only)
                + ", colorize: " + bool_to_string(m_colorize)
                + ", debug: " + bool_to_string(m_debug)
                + ", exclude_hidden: " + bool_to_string(m_exclude_hidden)
                + ", first_match: " + bool_to_string(m_first_match)
                + ", in_archive_extensions: " + string_vector_to_string(&m_in_archive_extensions)
                + ", in_archive_file_patterns: " + search_patterns_to_string(&m_in_archive_file_patterns)
                + ", in_dir_patterns: " + search_patterns_to_string(&m_in_dir_patterns)
                + ", in_extensions: " + string_vector_to_string(&m_in_extensions)
                + ", in_file_patterns: " + search_patterns_to_string(&m_in_file_patterns)
                + ", in_lines_after_patterns: " + search_patterns_to_string(&m_in_lines_after_patterns)
                + ", in_lines_before_patterns: " + search_patterns_to_string(&m_in_lines_before_patterns)
                + ", lines_after: " + std::to_string(m_lines_after)
                + ", lines_after_to_patterns: " + search_patterns_to_string(&m_lines_after_to_patterns)
                + ", lines_after_until_patterns: " + search_patterns_to_string(&m_lines_after_until_patterns)
                + ", lines_before: " + std::to_string(m_lines_before)
                + ", list_dirs: " + bool_to_string(m_list_dirs)
                + ", list_files: " + bool_to_string(m_list_files)
                + ", list_lines: " + bool_to_string(m_list_lines)
                + ", max_line_length: " + std::to_string(m_max_line_length)
                + ", multi_line_search: " + bool_to_string(m_multi_line_search)
                + ", out_archive_extensions: " + string_vector_to_string(&m_out_archive_extensions)
                + ", out_archive_file_patterns: " + search_patterns_to_string(&m_out_archive_file_patterns)
                + ", out_dir_patterns: " + search_patterns_to_string(&m_out_dir_patterns)
                + ", out_extensions: " + string_vector_to_string(&m_out_extensions)
                + ", out_file_patterns: " + search_patterns_to_string(&m_out_file_patterns)
                + ", out_lines_after_patterns: " + search_patterns_to_string(&m_out_lines_after_patterns)
                + ", out_lines_before_patterns: " + search_patterns_to_string(&m_out_lines_before_patterns)
                + ", paths: " + string_vector_to_string(&m_paths)
                + ", print_results: " + bool_to_string(m_print_results)
                + ", print_usage: " + bool_to_string(m_print_usage)
                + ", print_version: " + bool_to_string(m_print_version)
                + ", recursive: " + bool_to_string(m_recursive)
                + ", search_archives: " + bool_to_string(m_search_archives)
                + ", search_patterns: " + search_patterns_to_string(&m_search_patterns)
                + ", unique_lines: " + bool_to_string(m_unique_lines)
                + ", verbose: " + bool_to_string(m_verbose)
                + ")";
        return settings_str;
    }

    std::ostream& operator<<(std::ostream& strm, SearchSettings& settings) {
        std::string settings_string = settings.string();
        return strm << settings_string;
    }
}
