#include "SearchConfig.h"

namespace cppsearch {
    std::string get_xsearch_path() {
        const char* xs_path = std::getenv("XSEARCH_PATH");
        if (xs_path == nullptr) {
            const char* home = std::getenv("HOME");
            if (home == nullptr) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return std::string(home) + XSEARCH_REL_PATH;
        }
        return xs_path;
    }

    std::string get_search_options_path() {
        const std::string xs_path = get_xsearch_path();
        return xs_path + "/" + SEARCH_OPTIONS_REL_PATH;
    }

    std::string get_xsearch_config_dir() {
        const char* xs_config_dir = std::getenv("XSEARCH_CONFIG_DIR");
        if (xs_config_dir == nullptr) {
            const char* home = std::getenv("HOME");
            if (home == nullptr) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return std::string(home) + "/" + XSEARCH_CONFIG_REL_DIR;
        }
        return xs_config_dir;
    }

    std::string get_default_search_settings_path() {
        const std::string xs_config_dir = get_xsearch_config_dir();
        return xs_config_dir + "/settings.json";
    }

    SearchConfig::SearchConfig()
        : FindConfig()
        , m_xsearch_path(get_xsearch_path())
        , m_search_options_path(get_search_options_path())
        , m_default_search_settings_path(get_default_search_settings_path()) {
    }

    std::string SearchConfig::xsearch_path() const {
        return m_xsearch_path;
    }

    std::string SearchConfig::search_options_path() const {
        return m_search_options_path;
    }

    std::string SearchConfig::default_search_settings_path() const {
        return m_default_search_settings_path;
    }
}
