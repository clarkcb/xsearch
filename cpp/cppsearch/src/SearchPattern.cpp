#include "SearchPattern.h"

namespace cppsearch {
    SearchPattern::SearchPattern(const std::string& pattern) {
        m_pattern = pattern;
        m_regex = std::regex(pattern);
    }

    std::string SearchPattern::pattern() const {
        return m_pattern;
    }

    std::regex SearchPattern::r() const {
        return m_regex;
    }
}
