#ifndef CPPSEARCH_SEARCHRESULTFORMATTER_H
#define CPPSEARCH_SEARCHRESULTFORMATTER_H

#include <cstdlib>
#include "color.h"
#include "SearchResult.h"
#include "SearchSettings.h"
#include "SearchFile.h"
#include "StringUtil.h"

namespace cppsearch {
    class SearchResultFormatter {
    private:
        std::string format_matching_line(const SearchResult* result) const;
        std::string single_line_format(const SearchResult* result);
        std::string multi_line_format(const SearchResult* result);
        static long line_num_padding(const SearchResult* result);
        static std::string colorize(const std::string& s, unsigned long start_idx, unsigned long length);
        static const int line_sep_length = 80;

    public:
        const SearchSettings* settings;

        explicit SearchResultFormatter(const SearchSettings* settings);
        std::string format(const SearchResult* result);
    };
}

#endif //CPPSEARCH_SEARCHRESULTFORMATTER_H
