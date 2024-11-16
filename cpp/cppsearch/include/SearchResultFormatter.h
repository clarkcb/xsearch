#ifndef CPPSEARCH_SEARCHRESULTFORMATTER_H
#define CPPSEARCH_SEARCHRESULTFORMATTER_H

#include <cstdlib>
#include "color.h"
#include "SearchFileResult.h"
#include "SearchSettings.h"
#include "cppfind.h"

namespace cppsearch {
    class SearchResultFormatter {
    public:
        explicit SearchResultFormatter(const SearchSettings& settings);
        [[nodiscard]] SearchSettings settings() const;
        std::string format(const SearchFileResult& result) const;

    private:
        [[nodiscard]] std::string format_matching_line(const SearchFileResult& result) const;
        [[nodiscard]] std::string single_line_format(const SearchFileResult& result) const;
        [[nodiscard]] std::string multi_line_format(const SearchFileResult& result) const;
        static unsigned long line_num_padding(const SearchFileResult& result);
        static std::string colorize(const std::string& s, unsigned long start_idx, unsigned long length);

        static const int line_sep_length = 80;
        const SearchSettings m_settings;

    };
}

#endif //CPPSEARCH_SEARCHRESULTFORMATTER_H
