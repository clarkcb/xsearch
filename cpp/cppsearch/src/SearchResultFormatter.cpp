#include <boost/format.hpp>
#include "SearchResultFormatter.h"

namespace cppsearch {
    SearchResultFormatter::SearchResultFormatter(const SearchSettings& settings) :
    m_settings{settings},
    m_file_formatter{cppfind::FileResultFormatter(static_cast<cppfind::FindSettings>(settings))} {
        init();
    }

    SearchResultFormatter::SearchResultFormatter(const std::unique_ptr<SearchSettings>& settings_ptr) :
    m_settings{*settings_ptr},
    m_file_formatter{cppfind::FileResultFormatter(*settings_ptr)} {
        init();
    }

    void SearchResultFormatter::init() {
        if (m_settings.colorize()) {
            func_format_line = [this](const std::string& line) { return format_line_with_color(line); };
        } else {
            func_format_line = [](const std::string& line) { return line; };
        }
    }

    SearchSettings SearchResultFormatter::settings() const {
        return m_settings;
    }

    cppfind::FileResultFormatter SearchResultFormatter::file_formatter() const {
        return m_file_formatter;
    }

    std::string SearchResultFormatter::format(const SearchFileResult& result) const {
        if (m_settings.lines_before() > 0 || m_settings.lines_after() > 0) {
            return multi_line_format(result);
        }
        return single_line_format(result);
    }

    std::string SearchResultFormatter::format_matching_line(const SearchFileResult& result) const {
        auto formatted = cppfind::StringUtil::trim_copy(result.line());
        size_t formatted_length = formatted.length();
        const size_t leading_ws_count = cppfind::StringUtil::rtrim_copy(result.line()).length() - formatted_length;
        const size_t max_line_end_index = formatted_length - 1;
        const size_t match_length = result.match_end_idx() - result.match_start_idx();
        size_t match_start_index = result.match_start_idx() - 1 - leading_ws_count;
        size_t match_end_index = match_start_index + match_length;

        if (formatted_length > m_settings.max_line_length()) {
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
            while (formatted_length < m_settings.max_line_length()) {
                if (line_start_index > 0) {
                    line_start_index--;
                    match_start_index++;
                    match_end_index++;
                    formatted_length = line_end_index - line_start_index;
                }
                if (formatted_length < m_settings.max_line_length() && line_end_index < max_line_end_index) {
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

        // TODO: try using https://github.com/agauniyal/rang for CLI colorization
        if (m_settings.colorize()) {
            formatted = colorize(formatted, match_start_index, match_length);
        }
        return formatted;
    }

    std::string SearchResultFormatter::colorize(const std::string& s, const unsigned long start_idx,
                                                const unsigned long match_length) {
       return cppfind::FileResultFormatter::colorize(s, start_idx, match_length);
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
                    .append(format_matching_line(result));
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
                                     result.match_end_idx() - result.match_start_idx());
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
