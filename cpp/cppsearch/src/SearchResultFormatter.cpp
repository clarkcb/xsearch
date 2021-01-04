#include <boost/format.hpp>
#include "SearchResultFormatter.h"

namespace cppsearch {
    SearchResultFormatter::SearchResultFormatter(const SearchSettings* ss) {
        this->settings = ss;
    }

    std::string SearchResultFormatter::format_matching_line(const SearchResult* result) const {
        std::string formatted = StringUtil::trim_copy(result->line());
        size_t formatted_length = formatted.length();
        size_t leading_ws_count = StringUtil::rtrim_copy(result->line()).length() - formatted_length;
        size_t max_line_end_index = formatted_length - 1;
        size_t match_length = result->match_end_idx() - result->match_start_idx();
        size_t match_start_index = result->match_start_idx() - 1 - leading_ws_count;
        size_t match_end_index = match_start_index + match_length;

        if (formatted_length > settings->maxlinelength()) {
            size_t line_start_index = match_start_index;
            size_t line_end_index = match_start_index + match_length;
            match_start_index = 0;
            match_end_index = match_length;

            while (line_end_index > formatted_length - 1) {
                line_start_index--;
                line_end_index--;
                match_start_index++;
                match_end_index++;
            }

            formatted_length = line_end_index - line_start_index;
            while (formatted_length < this->settings->maxlinelength()) {
                if (line_start_index > 0) {
                    line_start_index--;
                    match_start_index++;
                    match_end_index++;
                    formatted_length = line_end_index - line_start_index;
                }
                if (formatted_length < settings->maxlinelength() && line_end_index < max_line_end_index) {
                    line_end_index++;
                }
                formatted_length = line_end_index - line_start_index;
            }

            formatted = formatted.substr(line_start_index, line_end_index - line_start_index);
            if (line_start_index > 2) {
                formatted = "..." + formatted.substr(3);
            }
            if (line_end_index < max_line_end_index - 3) {
                formatted = formatted.substr(0, formatted.length() - 3) + "...";
            }
        }

        if (settings->colorize()) {
            formatted = colorize(formatted, match_start_index, match_length);
//            formatted = formatted.substr(0, match_start_index) +
//                        COLOR_GREEN +
//                        formatted.substr(match_start_index, match_length) +
//                        COLOR_RESET +
//                        formatted.substr(match_start_index + match_length);
        }
        return formatted;
    }

    std::string SearchResultFormatter::colorize(const std::string& s, unsigned long start_idx, unsigned long match_length) {
        size_t s_size = s.size();
        std::string prefix = s.substr(0, start_idx);
        std::string match = s.substr(start_idx, match_length);
        return s.substr(0, start_idx) +
                    COLOR_GREEN +
                    s.substr(start_idx, match_length) +
                    COLOR_RESET +
                    s.substr(start_idx + match_length);
    }

    std::string SearchResultFormatter::single_line_format(const SearchResult* result) {
        std::string result_string = std::string(result->searchfile()->string());
        if (result->linenum() == 0) {
            result_string.append(" matches at [")
                    .append(std::to_string(result->match_start_idx()))
                    .append(":")
                    .append(std::to_string(result->match_end_idx()))
                    .append("]");
        } else {
            result_string.append(": ")
                    .append(std::to_string(result->linenum()))
                    .append(": [")
                    .append(std::to_string(result->match_start_idx()))
                    .append(":")
                    .append(std::to_string(result->match_end_idx()))
                    .append("]: ")
                    .append(format_matching_line(result));
        }
        return result_string;
    }

    long SearchResultFormatter::linenum_padding(const SearchResult* result) {
        unsigned long max_linenum = result->linenum() + result->lines_after().size();
        return boost::str(boost::format("%ld") % max_linenum).length();
    }

    std::string SearchResultFormatter::multi_line_format(const SearchResult* result) {
        //const int line_sep_length = 80;
        std::string result_string = std::string("================================================================================\n");
        if (result->searchfile() == nullptr) {
            result_string.append("<text>");
        } else {
            result_string.append(result->searchfile()->string());
        }
        result_string.append(": ")
                .append(std::to_string(result->linenum()))
                .append(": [")
                .append(std::to_string(result->match_start_idx()))
                .append(":").append(std::to_string(result->match_end_idx())).append("]\n");
        result_string.append("--------------------------------------------------------------------------------\n");
        unsigned long current_linenum = result->linenum();
        std::string line_format = " %1$";
        line_format.append(std::to_string(linenum_padding(result))).append("ld | %2$s\n");

        if (result->has_lines_before()) {
            current_linenum -= result->lines_before().size();
            for (const auto& line_before : result->lines_before()) {
                result_string.append(" ").append(boost::str(boost::format(line_format) % current_linenum % line_before));
                current_linenum++;
            }
        }
        std::string matching_line;
        if (settings->colorize()) {
            matching_line = colorize(result->line(), result->match_start_idx() - 1,
                                     result->match_end_idx() - result->match_start_idx());
        } else {
            matching_line = result->line();
        }
        result_string.append(">").append(boost::str(boost::format(line_format) % current_linenum % matching_line));
        current_linenum++;

        if (result->has_lines_after()) {
            for (const auto& line_after : result->lines_after()) {
                result_string.append(" ").append(boost::str(boost::format(line_format) % current_linenum % line_after));
                current_linenum++;
            }
        }

        return result_string;
    }

    std::string SearchResultFormatter::format(const SearchResult* result) {
        if (settings->linesbefore() > 0 || settings->linesafter() > 0) {
            return multi_line_format(result);
        } else {
            return single_line_format(result);
        }
    }
}
