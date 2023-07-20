#include <algorithm>
#include <cstdlib>
#include <deque>
#include <iostream>
#include <boost/format.hpp>
#include "rapidjson/filereadstream.h"
#include "config.h"
#include "SearchException.h"
#include "SearchOption.h"
#include "SearchOptions.h"
#include "cppfind.h"

namespace cppsearch {
    SearchOptions::SearchOptions() {
        m_coll_arg_map = {
                {"in-archiveext", [](std::string& s, SearchSettings* ss) { ss->add_in_archive_extension(s); }},
                {"in-archivefilepattern", [](std::string& s, SearchSettings* ss) { ss->add_in_archive_file_pattern(s); }},
                {"in-dirpattern", [](std::string& s, SearchSettings* ss) { ss->add_in_dir_pattern(s); }},
                {"in-ext", [](std::string& s, SearchSettings* ss) { ss->add_in_extension(s); }},
                {"in-filepattern", [](std::string& s, SearchSettings* ss) { ss->add_in_file_pattern(s); }},
                {"in-filetype", [](std::string& s, SearchSettings* ss) { auto t = cppfind::FileTypes::from_name(s); ss->add_in_file_type(t); }},
                {"in-linesafterpattern", [](std::string& s, SearchSettings* ss) { ss->add_in_lines_after_pattern(s); }},
                {"in-linesbeforepattern", [](std::string& s, SearchSettings* ss) { ss->add_in_lines_before_pattern(s); }},
                {"linesaftertopattern", [](std::string& s, SearchSettings* ss) { ss->add_lines_after_to_pattern(s); }},
                {"linesafteruntilpattern", [](std::string& s, SearchSettings* ss) { ss->add_lines_after_until_pattern(s); }},
                {"out-archiveext", [](std::string& s, SearchSettings* ss) { ss->add_out_archive_extension(s); }},
                {"out-archivefilepattern", [](std::string& s, SearchSettings* ss) { ss->add_out_archive_file_pattern(s); }},
                {"out-dirpattern", [](std::string& s, SearchSettings* ss) { ss->add_out_dir_pattern(s); }},
                {"out-ext", [](std::string& s, SearchSettings* ss) { ss->add_out_extension(s); }},
                {"out-filepattern", [](std::string& s, SearchSettings* ss) { ss->add_out_file_pattern(s); }},
                {"out-filetype", [](std::string& s, SearchSettings* ss) { auto t = cppfind::FileTypes::from_name(s); ss->add_out_file_type(t); }},
                {"out-linesafterpattern", [](std::string& s, SearchSettings* ss) { ss->add_out_lines_after_pattern(s); }},
                {"out-linesbeforepattern", [](std::string& s, SearchSettings* ss) { ss->add_out_lines_before_pattern(s); }},
                {"path", [](std::string& s, SearchSettings* ss) { ss->add_path(s); }},
                {"searchpattern", [](std::string& s, SearchSettings* ss) { ss->add_search_pattern(s); }},
                {"sort-by", [](std::string& s, SearchSettings* ss) { ss->set_sort_by(s); }}
        };

        m_int_arg_map = {
                {"linesafter", [](unsigned int i, SearchSettings* ss) { ss->lines_after(i); }},
                {"linesbefore", [](unsigned int i, SearchSettings* ss) { ss->lines_before(i); }},
                {"maxlinelength", [](unsigned int i, SearchSettings* ss) { ss->max_line_length(i); }}
        };

        m_str_arg_map = {
                {"maxlastmod", [](std::string& s, SearchSettings* ss) { ss->max_last_mod(cppfind::datestr_to_long(s)); }},
                {"maxsize", [](std::string& s, SearchSettings* ss) { ss->max_size(std::stol(s)); }},
                {"minlastmod", [](std::string& s, SearchSettings* ss) { ss->min_last_mod(cppfind::datestr_to_long(s)); }},
                {"minsize", [](std::string& s, SearchSettings* ss) { ss->min_size(std::stol(s)); }},
                {"settings-file", [this](std::string& s, SearchSettings* ss) { this->settings_from_file(s, ss); }}
        };

        m_bool_arg_map = {
                {"archivesonly", [](bool b, SearchSettings* ss) { ss->archives_only(b); }},
                {"allmatches", [](bool b, SearchSettings* ss) { ss->first_match(!b); }},
                {"colorize", [](bool b, SearchSettings* ss) { ss->colorize(b); }},
                {"debug", [](bool b, SearchSettings* ss) { ss->debug(b); }},
                {"excludearchives", [](bool b, SearchSettings* ss) { ss->include_archives(!b); }},
                {"excludehidden", [](bool b, SearchSettings* ss) { ss->exclude_hidden(b); }},
                {"firstmatch", [](bool b, SearchSettings* ss) { ss->first_match(b); }},
                {"help", [](bool b, SearchSettings* ss) { ss->print_usage(b); }},
                {"includearchives", [](bool b, SearchSettings* ss) { ss->include_archives(b); }},
                {"includehidden", [](bool b, SearchSettings* ss) { ss->exclude_hidden(!b); }},
                {"listdirs", [](bool b, SearchSettings* ss) { ss->list_dirs(b); }},
                {"listfiles", [](bool b, SearchSettings* ss) { ss->list_files(b); }},
                {"listlines", [](bool b, SearchSettings* ss) { ss->list_lines(b); }},
                {"multilinesearch", [](bool b, SearchSettings* ss) { ss->multi_line_search(b); }},
                {"nocolorize", [](bool b, SearchSettings* ss) { ss->colorize(!b); }},
                {"noprintmatches", [](bool b, SearchSettings* ss) { ss->print_results(!b); }},
                {"norecursive", [](bool b, SearchSettings* ss) { ss->recursive(!b); }},
                {"nosearcharchives", [](bool b, SearchSettings* ss) { ss->search_archives(!b); }},
                {"printmatches", [](bool b, SearchSettings* ss) { ss->print_results(b); }},
                {"recursive", [](bool b, SearchSettings* ss) { ss->recursive(b); }},
                {"searcharchives", [](bool b, SearchSettings* ss) { ss->search_archives(b); }},
                {"sort-ascending", [](bool b, SearchSettings* ss) { ss->sort_descending(!b); }},
                {"sort-caseinsensitive", [](bool b, SearchSettings* ss) { ss->sort_case_insensitive(b); }},
                {"sort-casesensitive", [](bool b, SearchSettings* ss) { ss->sort_case_insensitive(!b); }},
                {"sort-descending", [](bool b, SearchSettings* ss) { ss->sort_descending(b); }},
                {"uniquelines", [](bool b, SearchSettings* ss) { ss->unique_lines(b); }},
                {"verbose", [](bool b, SearchSettings* ss) { ss->verbose(b); }},
                {"version", [](bool b, SearchSettings* ss) { ss->print_version(b); }},
        };

        m_long_arg_map = {};
        m_options = {};
        load_options();
    }

    void SearchOptions::settings_from_file(std::string& file_path, SearchSettings* settings) {
        if (!cppfind::FileUtil::file_exists(file_path)) {
            std::string msg = "Settings file not found: ";
            msg.append(file_path);
            throw SearchException(msg);
        }

        FILE *fp = fopen(file_path.c_str(), "r");

        char readBuffer[65536];
        FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        Document document;
        document.ParseStream(is);
        fclose(fp);

        settings_from_document(&document, settings);
    }

    void SearchOptions::settings_from_json(std::string& json, SearchSettings* settings) {
        Document document;
        document.Parse(json.c_str());
        settings_from_document(&document, settings);
    }

    void SearchOptions::settings_from_document(Document* document, SearchSettings* settings) {
        assert(document->IsObject());

        for(Value::ConstMemberIterator it=document->MemberBegin(); it != document->MemberEnd(); it++) {
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
                m_int_arg_map[name]((unsigned int)i, settings);

            } else if (it->value.IsBool()) {
                assert(m_bool_arg_map.find(name) != m_bool_arg_map.end());
                bool b = it->value.GetBool();
                m_bool_arg_map[name](b, settings);

            } else if (it->value.IsString()) {
                auto* s = new std::string(it->value.GetString());
                if (m_str_arg_map.find(name) != m_str_arg_map.end()) {
                    m_str_arg_map[name](*s, settings);
                } else if (m_coll_arg_map.find(name) != m_coll_arg_map.end()) {
                    m_coll_arg_map[name](*s, settings);
                } else {
                    std::string msg = "Invalid option: " + name;
                    throw SearchException(msg);
                }
            }
        }
    }

    void SearchOptions::load_options() {
        auto search_options_path = std::string(XSEARCHPATH);
        search_options_path.append("/shared/searchoptions.json");

        if (!cppfind::FileUtil::file_exists(search_options_path)) {
            std::string msg = "Searchoptions file not found: ";
            msg.append(search_options_path);
            throw SearchException(msg);
        }

        FILE* fp = fopen(search_options_path.c_str(), "r");

        char readBuffer[65536];
        FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("searchoptions"));
        const Value& search_options = document["searchoptions"];
        assert(search_options.IsArray());
        for (SizeType i = 0; i < search_options.Size(); i++) {
            const Value::ConstObject &search_option = search_options[i].GetObject();
            assert(search_option.HasMember("long"));
            const Value &longValue = search_option["long"];
            auto* lng = new std::string(longValue.GetString());
            m_long_arg_map[*lng] = *lng;

            std::string* sht;
            if (search_option.HasMember("short")) {
                const Value &shortValue = search_option["short"];
                sht = new std::string(shortValue.GetString());
                m_long_arg_map[*sht] = *lng;
            } else {
                sht = new std::string("");
            }

            assert(search_option.HasMember("desc"));
            const Value &descValue = search_option["desc"];
            auto* desc = new std::string(descValue.GetString());

            auto* option = new SearchOption(sht, *lng, *desc);
            m_options.push_back(option);
        }
    }

    SearchSettings* SearchOptions::settings_from_args(int &argc, char **argv) {
        auto *settings = new SearchSettings();

        // set print results to true since we are running the executable
        settings->print_results(true);

        std::deque<std::string> arg_deque;
        unsigned int i;

        for (i=1; i < argc; i++) {
            auto arg = new std::string(argv[i]);
            arg_deque.push_back(*arg);
        }

        std::string* next_arg;
        while (!arg_deque.empty()) {
            next_arg = new std::string(arg_deque.front());
            arg_deque.pop_front();

            if ((*next_arg)[0] == '-') {
                while (!next_arg->empty() && (*next_arg)[0] == '-') {
                    *next_arg = next_arg->substr(1);
                }

                auto long_arg_found = m_long_arg_map.find(*next_arg);
                if (long_arg_found != m_long_arg_map.end()) {
                    auto long_arg = m_long_arg_map[*next_arg];

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
                            std::string msg = "Missing value for option ";
                            msg.append(*next_arg);
                            throw SearchException(msg);
                        } else {
                            auto* arg_val = new std::string(arg_deque.front());
                            arg_deque.pop_front();
                            if (coll_arg_found != m_coll_arg_map.end()) {
                                m_coll_arg_map[long_arg](*arg_val, settings);
                            } else if (int_arg_found != m_int_arg_map.end()) {
                                int int_val = stoi(*arg_val);
                                if (int_val < 0) {
                                    std::string msg = "Invalid value for option ";
                                    msg.append(*next_arg).append(": ").append(*arg_val);
                                    throw SearchException(msg);
                                }
                                m_int_arg_map[long_arg](int_val, settings);
                            } else if (str_arg_found != m_str_arg_map.end()) {
                                m_str_arg_map[long_arg](*arg_val, settings);
                            }
                        }
                    } else { // shouldn't be possible to get here
                        std::string msg = "Invalid option: ";
                        msg.append(*next_arg);
                        throw SearchException(msg);
                    }
                } else {
                    std::string msg = "Invalid option: ";
                    msg.append(*next_arg);
                    throw SearchException(msg);
                }
            } else {
                settings->add_path(*next_arg);
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
        auto* usage_string = new std::string("\nUsage:\n cppsearch [options] -s <searchpattern> <path> [<path> ...]\n\nOptions:\n");

        std::vector<std::string> opt_strings = {};
        std::vector<std::string> opt_descs = {};

        auto sort_option_lambda = [](const SearchOption* s1, const SearchOption* s2) -> bool {
            return s1->sort_arg().compare(s2->sort_arg()) < 0;
        };
        sort(m_options.begin(), m_options.end(), sort_option_lambda);

        long longest = 0;
        for (auto const& option : m_options) {
            std::string opt_string{};
            const std::string* short_arg = option->short_arg();
            if (!short_arg->empty()) {
                opt_string.append("-").append(*short_arg).append(",");
            }
//            const std::string long_arg = option->long_arg();
            opt_string.append("--").append(option->long_arg());
            if (opt_string.length() > longest) {
                longest = opt_string.length();
            }
            opt_strings.push_back(opt_string);
//            const std::string description = option->description();
            opt_descs.push_back(option->description());
        }

        std::string format = std::string(" %1$-") + std::to_string(longest) + "s  %2$s\n";
        for (int i = 0; i < opt_strings.size(); i++) {
            usage_string->append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
        }
        return *usage_string;
    }
}
