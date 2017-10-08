#ifndef CPPSEARCH_SEARCHPATTERN_H
#define CPPSEARCH_SEARCHPATTERN_H

#include <regex>

using namespace std;

class SearchPattern {
public:
    const string* pattern;
    regex r;

    explicit SearchPattern(const string* pattern);
};

#endif //CPPSEARCH_SEARCHPATTERN_H
