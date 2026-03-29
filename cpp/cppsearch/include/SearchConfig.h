#ifndef CPPSEARCH_CONFIG_H
#define CPPSEARCH_CONFIG_H

#include <string>

#define SEARCH_OPTIONS_REL_PATH "shared/searchoptions.json"
#define DEFAULT_SEARCH_SETTINGS_REL_PATH ".config/xsearch/settings.json"

namespace cppsearch {
    std::string xsearchpath();
    std::string default_search_settings_path();
}

#endif //CPPSEARCH_CONFIG_H
