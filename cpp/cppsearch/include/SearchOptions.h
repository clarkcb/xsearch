#ifndef CPPSEARCH_SEARCHOPTIONS_H
#define CPPSEARCH_SEARCHOPTIONS_H

#include <filesystem>
#include <functional>
#include <unordered_map>
#include <vector>

#include "SearchOption.h"
#include "SearchSettings.h"

namespace cppsearch {
    class SearchOptions {
    public:
        SearchOptions();
        SearchSettings settings_from_args(int &argc, char **argv);
        void settings_from_file(const std::filesystem::path& file_path, SearchSettings& settings);
        void settings_from_json(std::string_view json_str, SearchSettings& settings);
        void usage();
        std::string get_usage_string();

    private:
        std::unordered_map<std::string, std::function<void(bool, SearchSettings&)>> m_bool_arg_map;
        std::unordered_map<std::string, std::function<void(int, SearchSettings&)>> m_int_arg_map;
        std::unordered_map<std::string, std::function<void(uint64_t, SearchSettings&)>> m_long_arg_map;
        std::unordered_map<std::string, std::function<void(std::string&, SearchSettings&)>> m_str_arg_map;
        std::unordered_map<std::string, std::string> m_arg_name_map;
        std::vector<SearchOption> m_options;
        void load_options();
        void settings_from_document(rapidjson::Document& document, SearchSettings& settings);
    };
}

#endif //CPPSEARCH_SEARCHOPTIONS_H
