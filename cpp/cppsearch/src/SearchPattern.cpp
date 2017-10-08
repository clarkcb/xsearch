#include "SearchPattern.h"

SearchPattern::SearchPattern(const string* p) {
    pattern = p;
    r = regex(*p);
}
