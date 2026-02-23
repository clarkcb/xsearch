#include <boost/format.hpp>
#include "SearchResultFormatter.h"

namespace cppsearch {
    SearchResultFormatter::SearchResultFormatter(const SearchSettings& settings) :
    m_settings{settings},
    m_file_formatter{cppfind::FileResultFormatter(settings)} {
        init();
    }

    SearchResultFormatter::SearchResultFormatter(const std::unique_ptr<SearchSettings>& settings_ptr) :
    m_settings{*settings_ptr},
    m_file_formatter{cppfind::FileResultFormatter(*settings_ptr)} {
        init();
    }

    void SearchResultFormatter::init() {
    }

    SearchSettings SearchResultFormatter::settings() const {
        return m_settings;
    }

    std::string SearchResultFormatter::format(const SearchFileResult& result) const {
        if (m_settings.lines_before() > 0 || m_settings.lines_after() > 0) {
            return multi_line_format(result);
        }
        return single_line_format(result);
    }

    std::string SearchResultFormatter::format_result_match(const SearchFileResult& result) const {
        if (result.line().empty() || m_settings.max_line_length() == 0) {
            return "";
        }
        auto trimmed_line = cppfind::StringUtil::trim_copy(result.line());
        if (trimmed_line.empty()) {
            return "";
        }
        const size_t match_length = result.match_end_idx() - result.match_start_idx();
        size_t match_start_idx = result.match_start_idx() - 1;
        size_t match_end_idx = match_start_idx + match_length;

        std::string prefix;
        std::string suffix;
        size_t color_start_idx = 0;
        size_t color_end_idx = match_length;

        if (match_length > m_settings.max_line_length()) {
            if (m_settings.max_line_length() > 9) {
                if (match_start_idx > 2) {
                    prefix = "...";
                }
                suffix = "...";
            }

            color_start_idx = prefix.length();
            color_end_idx = m_settings.max_line_length() - suffix.length();
            match_end_idx = match_start_idx + color_end_idx;
            match_start_idx = match_start_idx + color_start_idx;
        }

        std::string match_string =
            prefix + result.line().substr(match_start_idx, match_end_idx - match_start_idx) + suffix;

        if (m_settings.colorize()) {
            return colorize(match_string, color_start_idx, color_end_idx - color_start_idx, settings().line_color());
        }

        return match_string;
    }

    std::string SearchResultFormatter::format_result_line(const SearchFileResult& result) const {
        if (result.line().empty() || settings().max_line_length() == 0) {
            return "";
        }

        if (result.match_end_idx() - result.match_start_idx() > m_settings.max_line_length()) {
            return format_result_match(result);
        }

        std::string line = result.line();
        size_t line_start_idx = cppfind::StringUtil::leading_whitespace(line);
        size_t line_end_idx = line.length() - cppfind::StringUtil::trailing_whitespace(line);

        const size_t match_length = result.match_end_idx() - result.match_start_idx();
        size_t match_start_idx = result.match_start_idx() - 1 - line_start_idx;
        size_t match_end_idx = match_start_idx + match_length;

        std::string prefix;
        std::string suffix;

        const size_t trimmed_length = line_end_idx - line_start_idx;

        if (trimmed_length == 0) {
            return "";
        }

        if (trimmed_length > m_settings.max_line_length()) {
            line_start_idx = result.match_start_idx() - 1;
            line_end_idx = line_start_idx + match_length;
            match_start_idx = 0;
            match_end_idx = match_length;

            size_t current_len = line_end_idx - line_start_idx;
            while (current_len < m_settings.max_line_length()) {
                if (line_start_idx > 0) {
                    line_start_idx--;
                    match_start_idx++;
                    match_end_idx++;
                    current_len++;
                }
                if (current_len < m_settings.max_line_length() && line_end_idx < trimmed_length) {
                    line_end_idx++;
                    current_len++;
                }
            }

            if (m_settings.max_line_length() > 9) {
                if (line_start_idx > 2) {
                    prefix = "...";
                    line_start_idx += 3;
                }
                if (line_end_idx < trimmed_length - 3) {
                    suffix = "...";
                    line_end_idx -= 3;
                }
            }
        } else {
            line_end_idx++;
        }

        std::string formatted_line = prefix + line.substr(line_start_idx, line_end_idx - line_start_idx) + suffix;

        // TODO: try using https://github.com/agauniyal/rang for CLI colorization
        if (m_settings.colorize()) {
            return colorize(formatted_line, match_start_idx, match_end_idx, settings().line_color());
        }

        return formatted_line;
    }

    std::string SearchResultFormatter::colorize(const std::string& s, const unsigned long match_start_idx,
                                                const unsigned long match_end_idx, const cppfind::Color color) {
       return cppfind::FileResultFormatter::colorize(s, match_start_idx, match_end_idx, color);
    }

    std::string SearchResultFormatter::single_line_format(const SearchFileResult& result) const {
        std::string result_string = std::string(result.file().string());
        if (result.line_num() == 0) {
            result_string.append(" matches at [")
                    .append(std::to_string(result.match_start_idx()))
                    .append(":")
                    .append(std::to_string(result.match_end_idx()))
                    .append("]");
        } else {
            result_string.append(": ")
                    .append(std::to_string(result.line_num()))
                    .append(": [")
                    .append(std::to_string(result.match_start_idx()))
                    .append(":")
                    .append(std::to_string(result.match_end_idx()))
                    .append("]: ")
                    .append(format_result_line(result));
        }
        return result_string;
    }

    unsigned long SearchResultFormatter::line_num_padding(const SearchFileResult& result) {
        unsigned long max_line_num = result.line_num() + result.lines_after().size();
        return boost::str(boost::format("%ld") % max_line_num).length();
    }

    std::string SearchResultFormatter::multi_line_format(const SearchFileResult& result) const {
        //const int line_sep_length = 80;
        auto result_string = std::string("================================================================================\n");
        result_string.append(result.file().string());
        result_string.append(": ")
                .append(std::to_string(result.line_num()))
                .append(": [")
                .append(std::to_string(result.match_start_idx()))
                .append(":").append(std::to_string(result.match_end_idx())).append("]\n");
        result_string.append("--------------------------------------------------------------------------------\n");
        unsigned long current_line_num = result.line_num();
        std::string line_format = " %1$";
        line_format.append(std::to_string(line_num_padding(result))).append("ld | %2$s\n");

        if (result.has_lines_before()) {
            current_line_num -= result.lines_before().size();
            for (const auto& line_before : result.lines_before()) {
                result_string.append(" ").append(boost::str(boost::format(line_format) % current_line_num % line_before));
                current_line_num++;
            }
        }
        std::string matching_line;
        if (m_settings.colorize()) {
            matching_line = colorize(result.line(), result.match_start_idx() - 1,
                                     result.match_end_idx() - result.match_start_idx(),
                                     settings().line_color());
        } else {
            matching_line = result.line();
        }
        result_string.append(">").append(boost::str(boost::format(line_format) % current_line_num % matching_line));
        current_line_num++;

        if (result.has_lines_after()) {
            for (const auto& line_after : result.lines_after()) {
                result_string.append(" ").append(boost::str(boost::format(line_format) % current_line_num % line_after));
                current_line_num++;
            }
        }

        return result_string;
    }
}
