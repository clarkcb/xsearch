#include <algorithm>
#include <deque>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <boost/format.hpp>

#include "rapidjson/filereadstream.h"

#include "SearchConfig.h"
#include "SearchException.h"
#include "SearchOption.h"
#include "SearchOptions.h"
#include "cppfind.h"

namespace cppsearch {
    SearchOptions::SearchOptions() {
        m_bool_arg_map = {
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

        m_int_arg_map = {
            {"linesafter", [](const int i, SearchSettings& ss) { ss.lines_after(i); }},
            {"linesbefore", [](const int i, SearchSettings& ss) { ss.lines_before(i); }},
            {"maxdepth", [](const int i, SearchSettings& ss) { ss.max_depth(i); }},
            {"maxlinelength", [](const int i, SearchSettings& ss) { ss.max_line_length(i); }},
            {"mindepth", [](const int i, SearchSettings& ss) { ss.min_depth(i); }},
        };

        m_long_arg_map = {
            {"maxsize", [](const uint64_t lng, SearchSettings& ss) { ss.max_size(lng); }},
            {"minsize", [](const uint64_t lng, SearchSettings& ss) { ss.min_size(lng); }},
        };

        m_str_arg_map = {
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
            {"settings-file", [this](std::string& s, SearchSettings& ss) { this->settings_from_file(s, ss); }},
            {"sort-by", [](const std::string& s, SearchSettings& ss) { ss.sort_by(cppfind::FindSettings::sort_by_from_name(s)); }},
        };

        load_options();
    }

    void SearchOptions::load_options() {
        auto search_options_path = std::filesystem::path(xsearchpath()) / "shared/searchoptions.json";

        if (!std::filesystem::exists(search_options_path)) {
            std::string msg{"Searchoptions file not found: "};
            msg.append(search_options_path);
            throw SearchException(msg);
        }

        uint64_t file_size = std::filesystem::file_size(search_options_path);
        // current size is 7260, make sure it's not dramatically bigger than that - 8192
        if (file_size > 8192) {
            throw SearchException("Invalid searchoptions file");
        }

        FILE* fp = fopen(search_options_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("searchoptions"));
        const rapidjson::Value& search_options = document["searchoptions"];
        assert(search_options.IsArray());
        for (rapidjson::SizeType i = 0; i < search_options.Size(); i++) {
            const rapidjson::Value::ConstObject &search_option = search_options[i].GetObject();
            assert(search_option.HasMember("long"));
            const rapidjson::Value &longValue = search_option["long"];
            auto long_arg = std::string(longValue.GetString());
            m_arg_name_map[long_arg] = long_arg;

            std::string short_arg;
            if (search_option.HasMember("short")) {
                const rapidjson::Value &shortValue = search_option["short"];
                short_arg = std::string(shortValue.GetString());
                m_arg_name_map[short_arg] = long_arg;
            } else {
                short_arg = std::string("");
            }

            assert(search_option.HasMember("desc"));
            const rapidjson::Value &descValue = search_option["desc"];
            auto desc = std::string(descValue.GetString());

            auto option = SearchOption(short_arg, long_arg, desc);
            m_options.push_back(std::move(option));
        }
    }

    // TODO: try using https://github.com/CLIUtils/CLI11 for CLI arg parsing
    SearchSettings SearchOptions::settings_from_args(int &argc, char **argv) {
        auto settings = SearchSettings();

        // set print results to true since we are running the executable
        settings.print_results(true);

        std::deque<std::string> arg_deque;
        unsigned int i;

        for (i=1; i < argc; ++i) {
            arg_deque.emplace_back(argv[i]);
        }

        std::string next_arg;
        while (!arg_deque.empty()) {
            next_arg = arg_deque.front();
            arg_deque.pop_front();

            if (next_arg[0] == '-') {
                while (!next_arg.empty() && next_arg[0] == '-') {
                    next_arg = next_arg.substr(1);
                }

                if (m_arg_name_map.contains(next_arg)) {
                    if (auto long_arg = m_arg_name_map[next_arg];
                        m_bool_arg_map.contains(long_arg)) {
                        m_bool_arg_map[long_arg](true, settings);
                    } else if (m_str_arg_map.contains(long_arg)
                        || m_int_arg_map.contains(long_arg)
                        || m_long_arg_map.contains(long_arg)) {
                        if (arg_deque.empty()) {
                            std::string msg{"Missing value for option "};
                            msg.append(next_arg);
                            throw SearchException(msg);
                        }
                        auto arg_val = arg_deque.front();
                        arg_deque.pop_front();
                        if (m_str_arg_map.contains(long_arg)) {
                            m_str_arg_map[long_arg](arg_val, settings);
                        } else if (m_int_arg_map.contains(long_arg)) {
                            const int int_val = std::stoi(arg_val);
                            m_int_arg_map[long_arg](int_val, settings);
                        } else if (m_long_arg_map.contains(long_arg)) {
                            const long long_val = std::stol(arg_val);
                            m_long_arg_map[long_arg](long_val, settings);
                        }
                    } else [[unlikely]] { // shouldn't be possible to get here
                        std::string msg{"Invalid option: "};
                        msg.append(next_arg);
                        throw SearchException(msg);
                    }
                } else {
                    std::string msg{"Invalid option: "};
                    msg.append(next_arg);
                    throw SearchException(msg);
                }
            } else {
                settings.add_path(next_arg);
            }
        }
        return settings;
    }

    void SearchOptions::settings_from_file(const std::filesystem::path& file_path, SearchSettings& settings) {
        if (!std::filesystem::exists(file_path)) {
            std::string msg{"Settings file not found: "};
            msg.append(file_path);
            throw SearchException(msg);
        }

        const uint64_t file_size = std::filesystem::file_size(file_path);
        // ~1MB, an arbitrary limit, but at least a limit
        assert(file_size <= 1024000);

        FILE *fp = fopen(file_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;
        document.ParseStream(is);
        fclose(fp);

        settings_from_document(document, settings);
    }

    void SearchOptions::settings_from_json(const std::string_view json_str, SearchSettings& settings) {
        rapidjson::Document document;
        document.Parse(std::string{json_str}.c_str());
        settings_from_document(document, settings);
    }

    void SearchOptions::settings_from_document(rapidjson::Document& document, SearchSettings& settings) {
        assert(document.IsObject());

        for (rapidjson::Value::ConstMemberIterator it=document.MemberBegin(); it != document.MemberEnd(); ++it) {
            std::string name = it->name.GetString();

            // TODO: we need to handle numeric types also
            if (it->value.IsArray()) {
                assert(m_str_arg_map.contains(name));
                const auto& arr = it->value.GetArray();
                for (rapidjson::SizeType i = 0; i < arr.Size(); ++i) {
                    assert(arr[i].IsString());
                    auto s = std::string(arr[i].GetString());
                    m_str_arg_map[name](s, settings);
                }

            } else if (it->value.IsBool()) {
                assert(m_bool_arg_map.contains(name));
                const bool b = it->value.GetBool();
                m_bool_arg_map[name](b, settings);

            } else if (it->value.IsString()) {
                auto s = std::string(it->value.GetString());
                if (m_str_arg_map.contains(name)) {
                    m_str_arg_map[name](s, settings);
                } else {
                    const std::string msg = "Invalid option: " + name;
                    throw SearchException(msg);
                }

            } else if (it->value.IsNumber()) {
                if (m_int_arg_map.contains(name)) {
                    m_int_arg_map[name](it->value.GetInt(), settings);
                } else if (m_long_arg_map.contains(name)) {
                    m_long_arg_map[name](it->value.GetUint64(), settings);
                } else {
                    const std::string msg = "Invalid option: " + name;
                    throw SearchException(msg);
                }
            }
        }
    }

    void SearchOptions::usage() {
        const std::string usage_string{get_usage_string()};
        std::cout << usage_string << std::endl;
        exit(1);
    }

    std::string SearchOptions::get_usage_string() {
        std::string usage_string;
        usage_string.reserve(3096);
        usage_string += "\nUsage:\n cppsearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n";

        std::vector<std::string> opt_strings{};
        std::vector<std::string> opt_descs{};

        auto sort_option_lambda = [](const SearchOption& s1, const SearchOption& s2) -> bool {
            return s1.sort_arg().compare(s2.sort_arg()) < 0;
        };
        std::ranges::sort(m_options, sort_option_lambda);

        unsigned long longest_len = 0;
        for (auto const& option : m_options) {
            std::string opt_string{};
            if (!option.short_arg().empty()) {
                opt_string.append("-").append(option.short_arg()).append(",");
            }
            opt_string.append("--").append(option.long_arg());
            if (opt_string.length() > longest_len) {
                longest_len = opt_string.length();
            }
            opt_strings.push_back(opt_string);
            opt_descs.push_back(option.description());
        }

        const std::string format = std::string(" %1$-") + std::to_string(longest_len) + "s  %2$s\n";
        for (int i = 0; i < opt_strings.size(); ++i) {
            usage_string.append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
        }
        return usage_string;
    }
}
