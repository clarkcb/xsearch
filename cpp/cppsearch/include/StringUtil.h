#ifndef CPPSEARCH_STRINGUTIL_H
#define CPPSEARCH_STRINGUTIL_H

#include <string>
#include <vector>

class StringUtil {
public:
    static std::vector<std::string> split_string(const std::string& s, const std::string& delims);
    static std::string trim_leading_whitespace(const std::string& s);

private:
    // Disallow creating an instance of this object
    StringUtil() = default;
};

#endif //CPPSEARCH_STRINGUTIL_H
