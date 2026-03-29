#include "SearchConfig.h"

namespace cppsearch {
    std::string xsearchpath() {
        std::string xsearchpath = std::getenv("XSEARCH_PATH");
        if (xsearchpath.empty()) {
            const std::string home = std::getenv("HOME");
            if (home.empty()) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return home + "/src/xsearch";
        }
        return xsearchpath;
    }

    std::string default_search_settings_path() {
        const std::string home = std::getenv("HOME");
        if (home.empty()) {
            // TODO: throw exception?
            return "";
        }
        // TODO: make this cross-platform
        return home + "/" + DEFAULT_SEARCH_SETTINGS_REL_PATH;
    }
}
