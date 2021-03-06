#ifndef CPPSEARCH_SEARCHPATTERN_H
#define CPPSEARCH_SEARCHPATTERN_H

#include <regex>

namespace cppsearch {
    class SearchPattern {
    private:
        std::string m_pattern;
        std::regex m_regex;

    public:
        explicit SearchPattern(const std::string& pattern);
        std::string pattern() const;
        std::regex r() const;
    };
}

#endif //CPPSEARCH_SEARCHPATTERN_H
