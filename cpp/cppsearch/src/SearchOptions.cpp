#include <algorithm>
#include <cstdlib>
#include <deque>
#include <iostream>
#include <FileUtil.h>
#include <SearchException.h>
#include <boost/format.hpp>
#include "rapidjson/filereadstream.h"
#include "common.h"
#include "config.h"
#include "FileTypes.h"
#include "SearchOption.h"
#include "SearchOptions.h"

using namespace std;

SearchOptions::SearchOptions() {

    coll_arg_map = {
            {"in-archiveext", [](string* s, SearchSettings* ss) { ss->add_in_archiveextension(s); }},
            {"in-archivefilepattern", [](string* s, SearchSettings* ss) { ss->add_in_archivefilepattern(s); }},
            {"in-dirpattern", [](string* s, SearchSettings* ss) { ss->add_in_dirpattern(s); }},
            {"in-ext", [](string* s, SearchSettings* ss) { ss->add_in_extension(s); }},
            {"in-filepattern", [](string* s, SearchSettings* ss) { ss->add_in_filepattern(s); }},
            {"in-filetype", [](string* s, SearchSettings* ss) { auto t = FileTypes::from_name(s); ss->add_in_filetype(&t); }},
            {"in-linesafterpattern", [](string* s, SearchSettings* ss) { ss->add_in_linesafterpattern(s); }},
            {"in-linesbeforepattern", [](string* s, SearchSettings* ss) { ss->add_in_linesbeforepattern(s); }},
            {"linesaftertopattern", [](string* s, SearchSettings* ss) { ss->add_linesaftertopattern(s); }},
            {"linesafteruntilpattern", [](string* s, SearchSettings* ss) { ss->add_linesafteruntilpattern(s); }},
            {"out-archiveext", [](string* s, SearchSettings* ss) { ss->add_out_archiveextension(s); }},
            {"out-archivefilepattern", [](string* s, SearchSettings* ss) { ss->add_out_archivefilepattern(s); }},
            {"out-dirpattern", [](string* s, SearchSettings* ss) { ss->add_out_dirpattern(s); }},
            {"out-ext", [](string* s, SearchSettings* ss) { ss->add_out_extension(s); }},
            {"out-filepattern", [](string* s, SearchSettings* ss) { ss->add_out_filepattern(s); }},
            {"out-filetype", [](string* s, SearchSettings* ss) { auto t = FileTypes::from_name(s); ss->add_out_filetype(&t); }},
            {"out-linesafterpattern", [](string* s, SearchSettings* ss) { ss->add_out_linesafterpattern(s); }},
            {"out-linesbeforepattern", [](string* s, SearchSettings* ss) { ss->add_out_linesbeforepattern(s); }},
            {"searchpattern", [](string* s, SearchSettings* ss) { ss->add_searchpattern(s); }},
            {"settings-file", [this](string* s, SearchSettings* ss) { this->settings_from_file(s, ss); }}
    };

    int_arg_map = {
            {"linesafter", [](int i, SearchSettings* ss) { ss->set_linesafter(i); }},
            {"linesbefore", [](int i, SearchSettings* ss) { ss->set_linesbefore(i); }},
            {"maxlinelength", [](int i, SearchSettings* ss) { ss->set_maxlinelength(i); }}
    };

    str_arg_map = {
            {"startpath", [](string* s, SearchSettings* ss) { ss->set_startpath(s); }},
            {"settings-file", [this](string* s, SearchSettings* ss) { this->settings_from_file(s, ss); }}
    };

    bool_arg_map = {
            {"archivesonly", [](bool b, SearchSettings* ss) { ss->set_archivesonly(b); }},
            {"allmatches", [](bool b, SearchSettings* ss) { ss->set_firstmatch(!b); }},
            {"debug", [](bool b, SearchSettings* ss) { ss->set_debug(b); }},
            {"excludehidden", [](bool b, SearchSettings* ss) { ss->set_excludehidden(b); }},
            {"firstmatch", [](bool b, SearchSettings* ss) { ss->set_firstmatch(b); }},
            {"help", [](bool b, SearchSettings* ss) { ss->set_printusage(b); }},
            {"includehidden", [](bool b, SearchSettings* ss) { ss->set_excludehidden(!b); }},
            {"listdirs", [](bool b, SearchSettings* ss) { ss->set_listdirs(b); }},
            {"listfiles", [](bool b, SearchSettings* ss) { ss->set_listfiles(b); }},
            {"listlines", [](bool b, SearchSettings* ss) { ss->set_listlines(b); }},
            {"multilinesearch", [](bool b, SearchSettings* ss) { ss->set_multilinesearch(b); }},
            {"noprintmatches", [](bool b, SearchSettings* ss) { ss->set_printresults(!b); }},
            {"norecursive", [](bool b, SearchSettings* ss) { ss->set_recursive(!b); }},
            {"nosearcharchives", [](bool b, SearchSettings* ss) { ss->set_searcharchives(!b); }},
            {"printmatches", [](bool b, SearchSettings* ss) { ss->set_printresults(b); }},
            {"recursive", [](bool b, SearchSettings* ss) { ss->set_recursive(b); }},
            {"searcharchives", [](bool b, SearchSettings* ss) { ss->set_searcharchives(b); }},
            {"uniquelines", [](bool b, SearchSettings* ss) { ss->set_uniquelines(b); }},
            {"verbose", [](bool b, SearchSettings* ss) { ss->set_verbose(b); }},
            {"version", [](bool b, SearchSettings* ss) { ss->set_printversion(b); }},
    };

    long_arg_map = {};
    options = {};
    load_options();
}

void SearchOptions::settings_from_file(string* filepath, SearchSettings* ss) {

}

void SearchOptions::load_options() {
    auto* searchoptions_path = new string(XSEARCHPATH);
    searchoptions_path->append("/shared/searchoptions.json");

    if (!FileUtil::file_exists(searchoptions_path)) {
        string msg = "Searchoptions file not found: ";
        msg.append(*searchoptions_path);
        throw SearchException(msg);
    }

    FILE* fp = fopen(searchoptions_path->c_str(), "r");

    char readBuffer[65536];
    FileReadStream is(fp, readBuffer, sizeof(readBuffer));

    Document document;

    document.ParseStream(is);
    fclose(fp);

    assert(document.HasMember("searchoptions"));
    const Value& searchoptions = document["searchoptions"];
    assert(searchoptions.IsArray());
    for (SizeType i = 0; i < searchoptions.Size(); i++) {
        const Value::ConstObject &searchoption = searchoptions[i].GetObject();
        assert(searchoption.HasMember("long"));
        const Value &longValue = searchoption["long"];
        auto* lng = new string(longValue.GetString());
        //printf("\nlong = \"%s\"\n", lng->c_str());
        long_arg_map[*lng] = *lng;

        string* sht;
        if (searchoption.HasMember("short")) {
            const Value &shortValue = searchoption["short"];
            sht = new string(shortValue.GetString());
            long_arg_map[*sht] = *lng;
        } else {
            sht = new string("");
        }
        //printf("short = \"%s\"\n", sht->c_str());

        assert(searchoption.HasMember("desc"));
        const Value &descValue = searchoption["desc"];
        auto* desc = new string(descValue.GetString());
        //printf("desc = \"%s\"\n", desc->c_str());

        auto* option = new SearchOption(sht, lng, desc);
        options.push_back(option);
    }
}

SearchSettings* SearchOptions::settings_from_args(int &argc, char **argv) {
    auto *settings = new SearchSettings();

    // set print results to true since we are running the executable
    settings->set_printresults(true);

    deque<string> arg_deque;
    unsigned int i;

    for (i=1; i < argc; i++) {
        auto arg = new string(argv[i]);
        arg_deque.push_back(*arg);
    }

    string* next_arg;
    while (!arg_deque.empty()) {
        next_arg = new string(arg_deque.front());
        arg_deque.pop_front();
        
        if ((*next_arg)[0] == '-') {
            while (!next_arg->empty() && (*next_arg)[0] == '-') {
                *next_arg = next_arg->substr(1);
            }

            auto long_arg_found = long_arg_map.find(*next_arg);
            if (long_arg_found != long_arg_map.end()) {
                auto longarg = long_arg_map[*next_arg];

                auto bool_arg_found = bool_arg_map.find(longarg);
                auto coll_arg_found = coll_arg_map.find(longarg);
                auto int_arg_found = int_arg_map.find(longarg);
                auto str_arg_found = str_arg_map.find(longarg);

                if (bool_arg_found != bool_arg_map.end()) {
                    bool_arg_map[longarg](true, settings);
                } else if (coll_arg_found != coll_arg_map.end()
                           || int_arg_found != int_arg_map.end()
                           || str_arg_found != str_arg_map.end()) {
                    if (arg_deque.empty()) {
                        string msg = "Missing value for option ";
                        msg.append(*next_arg);
                        throw SearchException(msg);
                    } else {
                        auto* arg_val = new string(arg_deque.front());
                        arg_deque.pop_front();
                        if (coll_arg_found != coll_arg_map.end()) {
                            coll_arg_map[longarg](arg_val, settings);
                        } else if (int_arg_found != int_arg_map.end()) {
                            int int_val = stoi(*arg_val);
                            if (int_val < 0) {
                                string msg = "Invalid value for option ";
                                msg.append(*next_arg).append(": ").append(*arg_val);
                                throw SearchException(msg);
                            }
                            int_arg_map[longarg](int_val, settings);
                        } else if (str_arg_found != str_arg_map.end()) {
                            str_arg_map[longarg](arg_val, settings);
                        }
                    }
                } else { // shouldn't be possible to get here
                    string msg = "Invalid option: ";
                    msg.append(*next_arg);
                    throw SearchException(msg);
                }
            } else {
                string msg = "Invalid option: ";
                msg.append(*next_arg);
                throw SearchException(msg);
            }
        } else {
            settings->set_startpath(next_arg);
        }
    }
    return settings;
}

void SearchOptions::usage() {
    string* usage_string = get_usage_string();
    cout << *usage_string << endl;
    exit(1);
}

string* SearchOptions::get_usage_string() {
    auto* usage_string = new string("\nUsage:\n cppsearch [options] -s <searchpattern> <startpath>\n\nOptions:\n");

    vector<string> opt_strings = {};
    vector<string> opt_descs = {};

    auto sort_option_lambda = [](const SearchOption* s1, const SearchOption* s2) -> bool {
        return s1->sortarg->compare(*s2->sortarg) < 0;
    };
    sort(options.begin(), options.end(), sort_option_lambda);

    long longest = 0;
    for (auto const& option : options) {
        string opt_string = string("");
        const string* shortarg = option->shortarg;
        if (!shortarg->empty()) {
            opt_string.append("-").append(*shortarg).append(",");
        }
        const string* longarg = option->longarg;
        opt_string.append("--").append(*longarg);
        if (opt_string.length() > longest) {
            longest = opt_string.length();
        }
        opt_strings.push_back(opt_string);
        const string* description = option->description;
        opt_descs.push_back(*description);
    }

    string format = string(" %1$-") + to_string(longest) + "s  %2$s\n";
    for (int i = 0; i < opt_strings.size(); i++) {
        usage_string->append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
    }
    return usage_string;
}
