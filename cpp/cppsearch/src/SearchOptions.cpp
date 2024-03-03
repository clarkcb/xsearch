#include <algorithm>
#include <cstdlib>
#include <deque>
#include <filesystem>
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
        m_coll_arg_map = {
                {"in-archiveext", [](const std::string& s, SearchSettings& ss) { ss.add_in_archive_extension(s); }},
                {"in-archivefilepattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_archive_file_pattern(s); }},
                {"in-dirpattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_dir_pattern(s); }},
                {"in-ext", [](const std::string& s, SearchSettings& ss) { ss.add_in_extension(s); }},
                {"in-filepattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_file_pattern(s); }},
                {"in-filetype", [](const std::string& s, SearchSettings& ss) { auto t = cppfind::FileTypes::from_name(s); ss.add_in_file_type(t); }},
                {"in-linesafterpattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_lines_after_pattern(s); }},
                {"in-linesbeforepattern", [](const std::string& s, SearchSettings& ss) { ss.add_in_lines_before_pattern(s); }},
                {"linesaftertopattern", [](const std::string& s, SearchSettings& ss) { ss.add_lines_after_to_pattern(s); }},
                {"linesafteruntilpattern", [](const std::string& s, SearchSettings& ss) { ss.add_lines_after_until_pattern(s); }},
                {"out-archiveext", [](const std::string& s, SearchSettings& ss) { ss.add_out_archive_extension(s); }},
                {"out-archivefilepattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_archive_file_pattern(s); }},
                {"out-dirpattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_dir_pattern(s); }},
                {"out-ext", [](const std::string& s, SearchSettings& ss) { ss.add_out_extension(s); }},
                {"out-filepattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_file_pattern(s); }},
                {"out-filetype", [](const std::string& s, SearchSettings& ss) { auto t = cppfind::FileTypes::from_name(s); ss.add_out_file_type(t); }},
                {"out-linesafterpattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_lines_after_pattern(s); }},
                {"out-linesbeforepattern", [](const std::string& s, SearchSettings& ss) { ss.add_out_lines_before_pattern(s); }},
                {"path", [](const std::string& s, SearchSettings& ss) { ss.add_path(s); }},
                {"searchpattern", [](const std::string& s, SearchSettings& ss) { ss.add_search_pattern(s); }},
                {"sort-by", [](const std::string& s, SearchSettings& ss) { ss.sort_by(cppfind::FindSettings::sort_by_from_name(s)); }}
        };

        m_int_arg_map = {
                {"linesafter", [](unsigned int i, SearchSettings& ss) { ss.lines_after(i); }},
                {"linesbefore", [](unsigned int i, SearchSettings& ss) { ss.lines_before(i); }},
                {"maxlinelength", [](unsigned int i, SearchSettings& ss) { ss.max_line_length(i); }}
        };

        m_str_arg_map = {
                {"maxdepth", [](const std::string& s, SearchSettings& ss) { ss.max_depth(std::stoi(s)); }},
                {"maxlastmod", [](const std::string& s, SearchSettings& ss) { ss.max_last_mod(cppfind::StringUtil::date_str_to_long(s)); }},
                {"maxsize", [](const std::string& s, SearchSettings& ss) { ss.max_size(std::stol(s)); }},
                {"mindepth", [](const std::string& s, SearchSettings& ss) { ss.min_depth(std::stoi(s)); }},
                {"minlastmod", [](const std::string& s, SearchSettings& ss) { ss.min_last_mod(cppfind::StringUtil::date_str_to_long(s)); }},
                {"minsize", [](const std::string& s, SearchSettings& ss) { ss.min_size(std::stol(s)); }},
                // {"settings-file", [this](std::string& s, SearchSettings& ss) { this->settings_from_file(s, ss); }}
        };

        m_bool_arg_map = {
                {"archivesonly", [](const bool b, SearchSettings& ss) { ss.search_archives_only(b); }},
                {"allmatches", [](const bool b, SearchSettings& ss) { ss.first_match(!b); }},
                {"colorize", [](const bool b, SearchSettings& ss) { ss.colorize(b); }},
                {"debug", [](const bool b, SearchSettings& ss) { ss.debug(b); }},
                {"excludearchives", [](const bool b, SearchSettings& ss) { ss.include_archives(!b); }},
                {"excludehidden", [](const bool b, SearchSettings& ss) { ss.include_hidden(!b); }},
                {"firstmatch", [](const bool b, SearchSettings& ss) { ss.first_match(b); }},
                {"help", [](const bool b, SearchSettings& ss) { ss.print_usage(b); }},
                {"includearchives", [](const bool b, SearchSettings& ss) { ss.include_archives(b); }},
                {"includehidden", [](const bool b, SearchSettings& ss) { ss.include_hidden(b); }},
                {"multilinesearch", [](const bool b, SearchSettings& ss) { ss.multi_line_search(b); }},
                {"nocolorize", [](const bool b, SearchSettings& ss) { ss.colorize(!b); }},
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

        m_long_arg_map = {};
        m_options = {};
        load_options();
    }

    void SearchOptions::settings_from_file(const std::filesystem::path& file_path, SearchSettings& settings) {
        if (!std::filesystem::exists(file_path)) {
            std::string msg = "Settings file not found: ";
            msg.append(file_path);
            throw SearchException(msg);
        }

        uint64_t file_size = std::filesystem::file_size(file_path);
        FILE *fp = fopen(file_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;
        document.ParseStream(is);
        fclose(fp);

        settings_from_document(document, settings);
    }

    void SearchOptions::settings_from_json(const std::string_view json, SearchSettings& settings) {
        rapidjson::Document document;
        document.Parse(std::string{json}.c_str());
        settings_from_document(document, settings);
    }

    void SearchOptions::settings_from_document(rapidjson::Document& document, SearchSettings& settings) {
        assert(document.IsObject());

        for (rapidjson::Value::ConstMemberIterator it=document.MemberBegin(); it != document.MemberEnd(); ++it) {
            std::string name = it->name.GetString();

            if (it->value.IsArray()) {
                assert(m_coll_arg_map.find(name) != m_coll_arg_map.end());
                const auto& arr = it->value.GetArray();
                for (SizeType i = 0; i < arr.Size(); i++) {
                    assert(arr[i].IsString());
                    auto* s = new std::string(arr[i].GetString());
                    m_coll_arg_map[name](*s, settings);
                }

            } else if (it->value.IsNumber()) {
                assert(m_int_arg_map.find(name) != m_int_arg_map.end());
                int i = it->value.GetInt();
                if (i < 0) {
                    std::string msg = "Invalid value for option ";
                    msg.append(name).append(": ").append(std::to_string(i));
                    throw SearchException(msg);
                }
                m_int_arg_map[name](static_cast<unsigned int>(i), settings);

            } else if (it->value.IsBool()) {
                assert(m_bool_arg_map.find(name) != m_bool_arg_map.end());
                const bool b = it->value.GetBool();
                m_bool_arg_map[name](b, settings);

            } else if (it->value.IsString()) {
//                auto* s = new std::string(it->value.GetString());
                auto s = std::string(it->value.GetString());
                if (m_str_arg_map.find(name) != m_str_arg_map.end()) {
                    m_str_arg_map[name](s, settings);
                } else if (m_coll_arg_map.find(name) != m_coll_arg_map.end()) {
                    m_coll_arg_map[name](s, settings);
                } else {
                    const std::string msg = "Invalid option: " + name;
                    throw SearchException(msg);
                }
            }
        }
    }

    void SearchOptions::load_options() {
        auto search_options_path = std::filesystem::path(xsearchpath()) / "shared/searchoptions.json";

        if (!std::filesystem::exists(search_options_path)) {
            std::string msg = "Searchoptions file not found: ";
            msg.append(search_options_path);
            throw SearchException(msg);
        }

        uint64_t file_size = std::filesystem::file_size(search_options_path);
        FILE* fp = fopen(search_options_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("searchoptions"));
        const rapidjson::Value& search_options = document["searchoptions"];
        assert(search_options.IsArray());
        for (SizeType i = 0; i < search_options.Size(); i++) {
            const rapidjson::Value::ConstObject &search_option = search_options[i].GetObject();
            assert(search_option.HasMember("long"));
            const rapidjson::Value &longValue = search_option["long"];
            auto lng = std::string(longValue.GetString());
            m_long_arg_map[lng] = lng;

            std::string sht;
            if (search_option.HasMember("short")) {
                const rapidjson::Value &shortValue = search_option["short"];
                sht = std::string(shortValue.GetString());
                m_long_arg_map[sht] = lng;
            } else {
                sht = std::string("");
            }

            assert(search_option.HasMember("desc"));
            const rapidjson::Value &descValue = search_option["desc"];
            auto desc = std::string(descValue.GetString());

            auto option = SearchOption(sht, lng, desc);
            m_options.push_back(std::move(option));
        }
    }

    SearchSettings SearchOptions::settings_from_args(int &argc, char **argv) {
        auto settings = SearchSettings();

        // set print results to true since we are running the executable
        settings.print_results(true);

        std::deque<std::string> arg_deque;
        unsigned int i;

        for (i=1; i < argc; i++) {
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

                auto long_arg_found = m_long_arg_map.find(next_arg);
                if (long_arg_found != m_long_arg_map.end()) {
                    auto long_arg = m_long_arg_map[next_arg];

                    auto bool_arg_found = m_bool_arg_map.find(long_arg);
                    auto coll_arg_found = m_coll_arg_map.find(long_arg);
                    auto int_arg_found = m_int_arg_map.find(long_arg);
                    auto str_arg_found = m_str_arg_map.find(long_arg);

                    if (bool_arg_found != m_bool_arg_map.end()) {
                        m_bool_arg_map[long_arg](true, settings);
                    } else if (coll_arg_found != m_coll_arg_map.end()
                               || int_arg_found != m_int_arg_map.end()
                               || str_arg_found != m_str_arg_map.end()) {
                        if (arg_deque.empty()) {
                            std::string msg{"Missing value for option "};
                            msg.append(next_arg);
                            throw SearchException(msg);
                        }
                        auto arg_val = arg_deque.front();
                        arg_deque.pop_front();
                        if (coll_arg_found != m_coll_arg_map.end()) {
                            m_coll_arg_map[long_arg](arg_val, settings);
                        } else if (int_arg_found != m_int_arg_map.end()) {
                            int int_val = stoi(arg_val);
                            if (int_val < 0) {
                                std::string msg{"Invalid value for option "};
                                msg.append(next_arg).append(": ").append(arg_val);
                                throw SearchException(msg);
                            }
                            m_int_arg_map[long_arg](int_val, settings);
                        } else if (str_arg_found != m_str_arg_map.end()) {
                            m_str_arg_map[long_arg](arg_val, settings);
                        }
                    } else { // shouldn't be possible to get here
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

    void SearchOptions::usage() {
        std::string usage_string = get_usage_string();
        std::cout << usage_string << std::endl;
        exit(1);
    }

    std::string SearchOptions::get_usage_string() {
        std::string usage_string{"\nUsage:\n cppsearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n"};

        std::vector<std::string> opt_strings{};
        std::vector<std::string> opt_descs{};

        auto sort_option_lambda = [](const SearchOption& s1, const SearchOption& s2) -> bool {
            return s1.sort_arg().compare(s2.sort_arg()) < 0;
        };
        std::sort(m_options.begin(), m_options.end(), sort_option_lambda);

        long longest_len = 0;
        for (auto const& option : m_options) {
            std::string opt_string{};
            std::string short_arg = option.short_arg();
            if (!short_arg.empty()) {
                opt_string.append("-").append(short_arg).append(",");
            }
            opt_string.append("--").append(option.long_arg());
            if (opt_string.length() > longest_len) {
                longest_len = opt_string.length();
            }
            opt_strings.push_back(opt_string);
            opt_descs.push_back(option.description());
        }

        const std::string format = std::string(" %1$-") + std::to_string(longest_len) + "s  %2$s\n";
        for (int i = 0; i < opt_strings.size(); i++) {
            usage_string.append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
        }
        return usage_string;
    }
}
