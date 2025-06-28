#ifndef CPPSEARCH_SEARCHRESULTFORMATTER_H
#define CPPSEARCH_SEARCHRESULTFORMATTER_H

#include <cstdlib>
#include "SearchFileResult.h"
#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {
    class SearchResultFormatter {
    public:
        explicit SearchResultFormatter(const SearchSettings& settings);
        explicit SearchResultFormatter(const std::unique_ptr<SearchSettings>& settings_ptr);
        SearchResultFormatter(SearchResultFormatter& other) = delete;
        SearchResultFormatter(SearchResultFormatter&& other) = delete;
        [[nodiscard]] SearchSettings settings() const;
        [[nodiscard]] cppfind::FileResultFormatter file_formatter() const;
        [[nodiscard]] std::string format(const SearchFileResult& result) const;

    private:
        void init();
        [[nodiscard]] std::string format_matching_line(const SearchFileResult& result) const;
        [[nodiscard]] std::string single_line_format(const SearchFileResult& result) const;
        [[nodiscard]] std::string multi_line_format(const SearchFileResult& result) const;
        static unsigned long line_num_padding(const SearchFileResult& result);
        static std::string colorize(const std::string& s, unsigned long start_idx, unsigned long length);
        [[nodiscard]] std::string format_line_with_color(const std::string& line) const;

        static const int line_sep_length = 80;
        const SearchSettings m_settings;
        const cppfind::FileResultFormatter m_file_formatter;
        std::function<std::string(const std::string&)> func_format_line;

    };
}

#endif //CPPSEARCH_SEARCHRESULTFORMATTER_H
