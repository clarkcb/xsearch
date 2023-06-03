#ifndef CPPSEARCH_SEARCHOPTIONS_H
#define CPPSEARCH_SEARCHOPTIONS_H

#include <unordered_map>
#include <vector>
#include "rapidjson/document.h"
#include "SearchOption.h"
#include "SearchSettings.h"

using namespace rapidjson;

namespace cppsearch {
    class SearchOptions {
    private:
        std::unordered_map<std::string, std::function<void(std::string&, SearchSettings*)>> m_coll_arg_map;
        std::unordered_map<std::string, std::function<void(bool, SearchSettings*)>> m_bool_arg_map;
        std::unordered_map<std::string, std::function<void(unsigned int, SearchSettings*)>> m_int_arg_map;
        std::unordered_map<std::string, std::function<void(std::string&, SearchSettings*)>> m_str_arg_map;
        std::unordered_map<std::string, std::string> m_long_arg_map;
        std::vector<SearchOption*> m_options;
        void load_options();
        void settings_from_file(std::string& file_path, SearchSettings* ss);
        void settings_from_document(Document* document, SearchSettings* settings);

    public:
        SearchOptions();
        SearchSettings* settings_from_args(int &argc, char **argv);
        void settings_from_json(std::string& json, SearchSettings* settings);
        void usage();
        std::string get_usage_string();
    };
}

#endif //CPPSEARCH_SEARCHOPTIONS_H
