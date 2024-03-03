#ifndef CPPSEARCH_SEARCHSETTINGS_H
#define CPPSEARCH_SEARCHSETTINGS_H

#include "cppfind.h"

namespace cppsearch {
    class SearchSettings : public cppfind::FindSettings {
    public:
        //using cppfind::FindSettings::FindSettings;

        SearchSettings();

        // property getters
        [[nodiscard]] bool colorize() const;
        [[nodiscard]] bool first_match() const;
        [[nodiscard]] bool multi_line_search() const;
        [[nodiscard]] unsigned int lines_after() const;
        [[nodiscard]] unsigned int lines_before() const;
        [[nodiscard]] size_t max_line_length() const;
        [[nodiscard]] bool print_lines() const;
        [[nodiscard]] bool print_results() const;
        [[nodiscard]] bool search_archives() const;
        [[nodiscard]] bool search_archives_only() const;
        [[nodiscard]] bool unique_lines() const;

        [[nodiscard]] std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> in_lines_after_patterns() const;
        [[nodiscard]] std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> in_lines_before_patterns() const;
        [[nodiscard]] std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> out_lines_after_patterns() const;
        [[nodiscard]] std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> out_lines_before_patterns() const;
        [[nodiscard]] std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> search_patterns() const;

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
        void search_archives_only(bool search_archives_only);
        void unique_lines(bool unique_lines);

        // add elements methods
        void add_in_lines_after_pattern(std::string_view pattern);
        void add_in_lines_before_pattern(std::string_view pattern);
        void add_lines_after_to_pattern(std::string_view pattern);
        void add_lines_after_until_pattern(std::string_view pattern);
        void add_out_lines_after_pattern(std::string_view pattern);
        void add_out_lines_before_pattern(std::string_view pattern);
        void add_search_pattern(std::string_view search_pattern);

        std::string string();

    private:
        bool m_colorize = true;
        bool m_first_match = false;

        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_in_lines_after_patterns;
        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_in_lines_before_patterns;

        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_lines_after_to_patterns;
        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_lines_after_until_patterns;

        unsigned int m_lines_after = 0;
        unsigned int m_lines_before = 0;
        size_t m_max_line_length = 150;
        bool m_multi_line_search = false;

        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_out_lines_after_patterns;
        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_out_lines_before_patterns;

        bool m_print_lines = false;
        bool m_print_results = false;
        bool m_search_archives = false;

        std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> m_search_patterns;

        bool m_unique_lines = false;

        // static std::string search_patterns_to_string(std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp>& patterns);
    };
}

#endif //CPPSEARCH_SEARCHSETTINGS_H
