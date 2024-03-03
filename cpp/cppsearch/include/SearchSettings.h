#ifndef CPPSEARCH_SEARCHSETTINGS_H
#define CPPSEARCH_SEARCHSETTINGS_H

#include <cstdlib>
#include "cppfind.h"

namespace cppsearch {
    class SearchSettings : public cppfind::FindSettings {
    private:
        bool m_colorize = true;
        bool m_first_match = false;

        std::vector<cppfind::RegexPattern*> m_in_lines_after_patterns;
        std::vector<cppfind::RegexPattern*> m_in_lines_before_patterns;

        std::vector<cppfind::RegexPattern*> m_lines_after_to_patterns;
        std::vector<cppfind::RegexPattern*> m_lines_after_until_patterns;

        unsigned int m_lines_after = 0;
        unsigned int m_lines_before = 0;
        bool m_list_lines = false;
        size_t m_max_line_length = 150;
        bool m_multi_line_search = false;

        std::vector<cppfind::RegexPattern*> m_out_lines_after_patterns;
        std::vector<cppfind::RegexPattern*> m_out_lines_before_patterns;

        bool m_print_results = false;
        bool m_search_archives = false;

        std::vector<cppfind::RegexPattern*> m_search_patterns;

        bool m_unique_lines = false;

        static std::string search_patterns_to_string(std::vector<cppfind::RegexPattern*>* ps);

    public:
        SearchSettings();
        using cppfind::FindSettings::FindSettings;
        void add_in_lines_after_pattern(const std::string& pattern);
        void add_in_lines_before_pattern(const std::string& pattern);
        void add_lines_after_to_pattern(const std::string& pattern);
        void add_lines_after_until_pattern(const std::string& pattern);
        void add_out_lines_after_pattern(const std::string& pattern);
        void add_out_lines_before_pattern(const std::string& pattern);
        void add_search_pattern(const std::string& search_pattern);

        [[nodiscard]] bool archives_only() const;
        [[nodiscard]] bool colorize() const;
        [[nodiscard]] bool first_match() const;
        [[nodiscard]] bool multi_line_search() const;
        [[nodiscard]] unsigned int lines_after() const;
        [[nodiscard]] unsigned int lines_before() const;
        [[nodiscard]] size_t max_line_length() const;
        [[nodiscard]] bool print_lines() const;
        [[nodiscard]] bool print_results() const;
        [[nodiscard]] bool search_archives() const;
        [[nodiscard]] bool unique_lines() const;

        std::vector<cppfind::RegexPattern*>* in_lines_after_patterns();
        std::vector<cppfind::RegexPattern*>* in_lines_before_patterns();

        std::vector<cppfind::RegexPattern*>* out_lines_after_patterns();
        std::vector<cppfind::RegexPattern*>* out_lines_before_patterns();

        std::vector<cppfind::RegexPattern*>* search_patterns();

        // property setters
        void colorize(bool colorize);
        void first_match(bool first_match);
        void multi_line_search(bool multi_line_search);
        void lines_after(unsigned int line_count);
        void lines_before(unsigned int line_count);
        void max_line_length(size_t max);
        void print_lines(bool print_lines);
        void print_results(bool print_results);
        void search_archives(bool search_archives);
        void unique_lines(bool unique_lines);
        std::string string();
    };
}

#endif //CPPSEARCH_SEARCHSETTINGS_H
