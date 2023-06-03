#ifndef CPPSEARCH_SEARCHSETTINGS_H
#define CPPSEARCH_SEARCHSETTINGS_H

#include <cstdlib>
#include "FileTypes.h"
#include "SearchPattern.h"

namespace cppsearch {
    class SearchSettings {
    private:
        bool m_archives_only = false;
        bool m_colorize = true;
        bool m_debug = false;
        bool m_exclude_hidden = true;
        bool m_first_match = false;

        std::vector<std::string> m_in_archive_extensions;
        std::vector<SearchPattern*> m_in_archive_file_patterns;
        std::vector<SearchPattern*> m_in_dir_patterns;
        std::vector<std::string> m_in_extensions;
        std::vector<SearchPattern*> m_in_file_patterns;
        std::vector<FileType> m_in_file_types;

        std::vector<SearchPattern*> m_in_lines_after_patterns;
        std::vector<SearchPattern*> m_in_lines_before_patterns;

        std::vector<SearchPattern*> m_lines_after_to_patterns;
        std::vector<SearchPattern*> m_lines_after_until_patterns;

        unsigned int m_lines_after = 0;
        unsigned int m_lines_before = 0;
        bool m_list_dirs = false;
        bool m_list_files = false;
        bool m_list_lines = false;
        size_t m_max_line_length = 150;
        bool m_multi_line_search = false;

        std::vector<std::string> m_out_archive_extensions;
        std::vector<SearchPattern*> m_out_archive_file_patterns;
        std::vector<SearchPattern*> m_out_dir_patterns;
        std::vector<std::string> m_out_extensions;
        std::vector<SearchPattern*> m_out_file_patterns;
        std::vector<FileType> m_out_file_types;

        std::vector<SearchPattern*> m_out_lines_after_patterns;
        std::vector<SearchPattern*> m_out_lines_before_patterns;

        std::vector<std::string> m_paths;

        bool m_print_results = false;
        bool m_print_usage = false;
        bool m_print_version = false;
        bool m_recursive = true;
        bool m_search_archives = false;

        std::vector<SearchPattern*> m_search_patterns;

        bool m_unique_lines = false;
        bool m_verbose = false;

        static std::string bool_to_string(bool b);
        static std::string string_vector_to_string(std::vector<std::string>* s);
        static std::string search_patterns_to_string(std::vector<SearchPattern*>* ps);

        static void add_pattern(const std::string& p, std::vector<SearchPattern*>* ps);
        static void add_extensions(const std::string& exts, std::vector<std::string>* extensions);

    public:
        SearchSettings();
        void add_in_archive_extension(const std::string& ext);
        void add_in_archive_file_pattern(const std::string& pattern);
        void add_in_dir_pattern(const std::string& pattern);
        void add_in_extension(const std::string& ext);
        void add_in_file_pattern(const std::string& pattern);
        void add_in_file_type(FileType file_type);
        void add_in_lines_after_pattern(const std::string& pattern);
        void add_in_lines_before_pattern(const std::string& pattern);
        void add_lines_after_to_pattern(const std::string& pattern);
        void add_lines_after_until_pattern(const std::string& pattern);
        void add_out_archive_extension(const std::string& ext);
        void add_out_archive_file_pattern(const std::string& pattern);
        void add_out_dir_pattern(const std::string& pattern);
        void add_out_extension(const std::string& ext);
        void add_out_file_pattern(const std::string& pattern);
        void add_out_file_type(FileType file_type);
        void add_out_lines_after_pattern(const std::string& pattern);
        void add_out_lines_before_pattern(const std::string& pattern);
        void add_path(const std::string& path);
        void add_search_pattern(const std::string& search_pattern);

        [[nodiscard]] bool archives_only() const;
        [[nodiscard]] bool colorize() const;
        [[nodiscard]] bool debug() const;
        [[nodiscard]] bool exclude_hidden() const;
        [[nodiscard]] bool first_match() const;
        [[nodiscard]] bool multi_line_search() const;
        [[nodiscard]] unsigned int lines_after() const;
        [[nodiscard]] unsigned int lines_before() const;
        [[nodiscard]] bool list_dirs() const;
        [[nodiscard]] bool list_files() const;
        [[nodiscard]] bool list_lines() const;
        [[nodiscard]] size_t max_line_length() const;
        [[nodiscard]] bool print_results() const;
        [[nodiscard]] bool print_usage() const;
        [[nodiscard]] bool print_version() const;
        [[nodiscard]] bool recursive() const;
        [[nodiscard]] bool search_archives() const;
        [[nodiscard]] bool unique_lines() const;
        [[nodiscard]] bool verbose() const;

        std::vector<std::string>* in_archive_extensions();
        std::vector<SearchPattern*>* in_archive_file_patterns();
        std::vector<SearchPattern*>* in_dir_patterns();
        std::vector<std::string>* in_extensions();
        std::vector<SearchPattern*>* in_file_patterns();
        std::vector<SearchPattern*>* in_lines_after_patterns();
        std::vector<SearchPattern*>* in_lines_before_patterns();

        std::vector<std::string>* out_archive_extensions();
        std::vector<SearchPattern*>* out_archive_file_patterns();
        std::vector<SearchPattern*>* out_dir_patterns();
        std::vector<std::string>* out_extensions();
        std::vector<SearchPattern*>* out_file_patterns();
        std::vector<SearchPattern*>* out_lines_after_patterns();
        std::vector<SearchPattern*>* out_lines_before_patterns();

        std::vector<std::string>* paths();
        std::vector<SearchPattern*>* search_patterns();

        // bool is_in_archive_extension(const std::string* ext);
        // bool is_in_extension(const std::string* ext);
        // bool is_in_file_type(const FileType* m_file_type);
        // bool is_out_archive_extension(const std::string* ext);
        // bool is_out_extension(const std::string* ext);

        void archives_only(bool b);
        void colorize(bool b);
        void debug(bool b);
        void exclude_hidden(bool b);
        void first_match(bool b);
        void multi_line_search(bool b);
        void lines_after(unsigned int line_count);
        void lines_before(unsigned int line_count);
        void list_dirs(bool b);
        void list_files(bool b);
        void list_lines(bool b);
        void max_line_length(size_t max);
        void print_results(bool b);
        void print_usage(bool b);
        void print_version(bool b);
        void recursive(bool b);
        void search_archives(bool b);
        void unique_lines(bool b);
        void verbose(bool b);
        std::string string();
    };
}

#endif //CPPSEARCH_SEARCHSETTINGS_H
