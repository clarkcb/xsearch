#ifndef CPPSEARCH_SEARCHOPTIONS_H
#define CPPSEARCH_SEARCHOPTIONS_H

#include <unordered_map>
#include <vector>
#include "rapidjson/document.h"
#include "SearchOption.h"
#include "SearchSettings.h"

using namespace rapidjson;

class SearchOptions {
public:
    SearchOptions();
    SearchSettings* settings_from_args(int &argc, char **argv);
    void usage();
    string* get_usage_string();

private:
    unordered_map<string, std::function<void(string*, SearchSettings*)>> coll_arg_map;
    unordered_map<string, std::function<void(bool, SearchSettings*)>> bool_arg_map;
    unordered_map<string, std::function<void(unsigned int, SearchSettings*)>> int_arg_map;
    unordered_map<string, std::function<void(string*, SearchSettings*)>> str_arg_map;
    unordered_map<string, string> long_arg_map;
    vector<SearchOption*> options;
    void load_options();
    void settings_from_file(string* filepath, SearchSettings* ss);
    void settings_from_json(string* json, SearchSettings* settings);
    void settings_from_document(Document* document, SearchSettings* settings);
};

#endif //CPPSEARCH_SEARCHOPTIONS_H
