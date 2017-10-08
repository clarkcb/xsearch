#include <algorithm/string.hpp>
#include "SearchOption.h"

using namespace std;

SearchOption::SearchOption(const string* sa, const string* la, const string* desc) {
    shortarg = sa;
    longarg = la;
    description = desc;
    if (shortarg != nullptr && !shortarg->empty()) {
        string* so = new string(boost::to_lower_copy(*shortarg));
        so->append("@");
        so->append(*longarg);
        sortarg = so;
    } else {
        sortarg = longarg;
    }
}
