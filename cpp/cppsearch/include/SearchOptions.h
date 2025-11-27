#ifndef CPPSEARCH_SEARCHOPTIONS_H
#define CPPSEARCH_SEARCHOPTIONS_H

#include <filesystem>
#include <functional>
#include <unordered_map>
#include <vector>

#include "cppfind.h"
#include "SearchSettings.h"

namespace cppsearch {
    class SearchOptions {
    public:
        SearchOptions();
        SearchSettings settings_from_args(int &argc, char **argv);
        void update_settings_from_args(SearchSettings& settings, int &argc, char **argv);
        void update_settings_from_file(SearchSettings& settings, const std::filesystem::path& file_path);
        void update_settings_from_json(SearchSettings& settings, std::string_view json_str);
        void usage();
        std::string get_usage_string();

    private:
        std::unordered_map<std::string, std::function<void(bool, SearchSettings&)>> m_bool_arg_map = {
            {"archivesonly", [](const bool b, SearchSettings& ss) { ss.archives_only(b); }},
            {"allmatches", [](const bool b, SearchSettings& ss) { ss.first_match(!b); }},
            {"colorize", [](const bool b, SearchSettings& ss) { ss.colorize(b); }},
            {"debug", [](const bool b, SearchSettings& ss) { ss.debug(b); }},
            {"excludearchives", [](const bool b, SearchSettings& ss) { ss.include_archives(!b); }},
            {"excludehidden", [](const bool b, SearchSettings& ss) { ss.include_hidden(!b); }},
            {"firstmatch", [](const bool b, SearchSettings& ss) { ss.first_match(b); }},
            {"followsymlinks", [](const bool b, SearchSettings& ss) { ss.follow_symlinks(b); }},
            {"help", [](const bool b, SearchSettings& ss) { ss.print_usage(b); }},
            {"includearchives", [](const bool b, SearchSettings& ss) { ss.include_archives(b); }},
            {"includehidden", [](const bool b, SearchSettings& ss) { ss.include_hidden(b); }},
            {"multilinesearch", [](const bool b, SearchSettings& ss) { ss.multi_line_search(b); }},
            {"nocolorize", [](const bool b, SearchSettings& ss) { ss.colorize(!b); }},
            {"nofollowsymlinks", [](const bool b, SearchSettings& ss) { ss.follow_symlinks(!b); }},
            {"noprintdirs", [](const bool b, SearchSettings& ss) { ss.print_dirs(!b); }},
            {"noprintfiles", [](const bool b, SearchSettings& ss) { ss.print_files(!b); }},
            {"noprintlines", [](const bool b, SearchSettings& ss) { ss.print_lines(!b); }},
            {"noprintmatches", [](const bool b, SearchSettings& ss) { ss.print_results(!b); }},
            {"norecursive", [](const bool b, SearchSettings& ss) { ss.recursive(!b); }},
            {"nosearcharchives", [](const bool b, SearchSettings& ss) { ss.search_archives(!b); }},
            {"printdirs", [](const bool b, SearchSettings& ss) { ss.print_dirs(b); }},
            {"printfiles", [](const bool b, SearchSettings& ss) { ss.print_files(b); }},
            {"printlines", [](const bool b, SearchSettings& ss) { ss.print_lines(b); }},
            {"printmatches", [](const bool b, SearchSettings& ss) { ss.print_results(b); }},
            {"recursive", [](const bool b, SearchSettings& ss) { ss.recursive(b); }},
            {"searcharchives", [](const bool b, SearchSettings& ss) { ss.search_archives(b); }},
            {"sort-ascending", [](const bool b, SearchSettings& ss) { ss.sort_descending(!b); }},
            {"sort-caseinsensitive", [](const bool b, SearchSettings& ss) { ss.sort_case_insensitive(b); }},
            {"sort-casesensitive", [](const bool b, SearchSettings& ss) { ss.sort_case_insensitive(!b); }},
            {"sort-descending", [](const bool b, SearchSettings& ss) { ss.sort_descending(b); }},
            {"uniquelines", [](const bool b, SearchSettings& ss) { ss.unique_lines(b); }},
            {"verbose", [](const bool b, SearchSettings& ss) { ss.verbose(b); }},
            {"version", [](const bool b, SearchSettings& ss) { ss.print_version(b); }},
        };

        std::unordered_map<std::string, std::function<void(int, SearchSettings&)>> m_int_arg_map = {
            {"linesafter", [](const int i, SearchSettings& ss) { ss.lines_after(i); }},
            {"linesbefore", [](const int i, SearchSettings& ss) { ss.lines_before(i); }},
            {"maxdepth", [](const int i, SearchSettings& ss) { ss.max_depth(i); }},
            {"maxlinelength", [](const int i, SearchSettings& ss) { ss.max_line_length(i); }},
            {"mindepth", [](const int i, SearchSettings& ss) { ss.min_depth(i); }},
        };

        std::unordered_map<std::string, std::function<void(uint64_t, SearchSettings&)>> m_long_arg_map = {
            {"maxsize", [](const uint64_t lng, SearchSettings& ss) { ss.max_size(lng); }},
            {"minsize", [](const uint64_t lng, SearchSettings& ss) { ss.min_size(lng); }},
        };

        std::unordered_map<std::string, std::function<void(std::string&, SearchSettings&)>> m_str_arg_map = {
            {"in-archiveext", [](const std::string& s, SearchSettings& ss) { ss.add_in_archive_extension(s); }},
            {"in-archivefilepattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_archive_file_pattern(s); }},
            {"in-dirpattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_dir_pattern(s); }},
            {"in-ext", [](const std::string& s, SearchSettings& ss) { ss.add_in_extension(s); }},
            {"in-filepattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_file_pattern(s); }},
            {"in-filetype", [](const std::string& s, SearchSettings& ss) { ss.add_in_file_type(cppfind::FileTypes::from_name(s)); }},
            {"in-linesafterpattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_lines_after_pattern(s); }},
            {"in-linesbeforepattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_lines_before_pattern(s); }},
            {"linesaftertopattern", [](const std::string& s, SearchSettings& ss) { ss.add_lines_after_to_pattern(s); }},
            {"linesafteruntilpattern", [](const std::string& s, SearchSettings& ss) { ss.add_lines_after_until_pattern(s); }},
            {"maxlastmod", [](const std::string& s, SearchSettings& ss) { ss.max_last_mod(cppfind::StringUtil::date_str_to_long(s)); }},
            {"minlastmod", [](const std::string& s, SearchSettings& ss) { ss.min_last_mod(cppfind::StringUtil::date_str_to_long(s)); }},
            {"out-archiveext", [](const std::string& s, SearchSettings& ss) { ss.add_out_archive_extension(s); }},
            {"out-archivefilepattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_archive_file_pattern(s); }},
            {"out-dirpattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_dir_pattern(s); }},
            {"out-ext", [](const std::string& s, SearchSettings& ss) { ss.add_out_extension(s); }},
            {"out-filepattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_file_pattern(s); }},
            {"out-filetype", [](const std::string& s, SearchSettings& ss) { ss.add_out_file_type(cppfind::FileTypes::from_name(s)); }},
            {"out-linesafterpattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_lines_after_pattern(s); }},
            {"out-linesbeforepattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_lines_before_pattern(s); }},
            {"path", [](const std::string& s, SearchSettings& ss) { ss.add_path(s); }},
            {"searchpattern", [](const std::string& s, SearchSettings& ss) { ss.add_search_pattern(s); }},
            {"settings-file", [this](std::string& s, SearchSettings& ss) { this->update_settings_from_file(ss, s); }},
            {"sort-by", [](const std::string& s, SearchSettings& ss) { ss.sort_by(cppfind::FindSettings::sort_by_from_name(s)); }},
        };

        std::unordered_map<std::string, std::string> m_arg_name_map = {
            {"path", "path"},
        };

        std::vector<std::unique_ptr<cppfind::Option>> m_options;
        cppfind::ArgTokenizer m_arg_tokenizer;
        std::vector<std::unique_ptr<cppfind::Option>> load_options();
        void update_settings_from_arg_token(SearchSettings& settings, const cppfind::ArgToken& arg_tokens);
        void update_settings_from_arg_tokens(SearchSettings& settings, const std::vector<cppfind::ArgToken>& arg_tokens);
    };
}

#endif //CPPSEARCH_SEARCHOPTIONS_H
