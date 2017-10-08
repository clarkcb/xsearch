#ifndef CPPSEARCH_SEARCHOPTIONS_H
#define CPPSEARCH_SEARCHOPTIONS_H

#include <unordered_map>
#include <vector>
#include "SearchOption.h"
#include "SearchSettings.h"

class SearchOptions {
public:
    SearchOptions();
    SearchSettings* settings_from_args(int &argc, char **argv);
    void usage();
    string* get_usage_string();

private:
    unordered_map<string, std::function<void(string*, SearchSettings*)>> arg_action_map;
    unordered_map<string, std::function<void(bool, SearchSettings*)>> flag_action_map;
    unordered_map<string, string> long_arg_map;
    vector<SearchOption*> options;
    void load_options();
    void settings_from_file(string* filepath, SearchSettings* ss);
};

#endif //CPPSEARCH_SEARCHOPTIONS_H
