#ifndef CPPSEARCH_CONFIG_H
#define CPPSEARCH_CONFIG_H

#include <string>
#include "cppfind.h"

#define XSEARCH_REL_PATH "src/xsearch"
#define SEARCH_OPTIONS_REL_PATH "shared/searchoptions.json"
#define XSEARCH_CONFIG_REL_DIR ".config/xsearch"

namespace cppsearch {
    class SearchConfig : public cppfind::FindConfig {
    public:
        SearchConfig();
        [[nodiscard]] std::string xsearch_path() const;
        [[nodiscard]] std::string search_options_path() const;
        [[nodiscard]] std::string default_search_settings_path() const;

    private:
        std::string m_xsearch_path;
        std::string m_search_options_path;
        std::string m_default_search_settings_path;
    };
}

#endif //CPPSEARCH_CONFIG_H
