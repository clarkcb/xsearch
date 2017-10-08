#include <boost/algorithm/string.hpp>
#include "StringUtil.h"

std::vector<std::string> StringUtil::split_string(const std::string& s, const std::string& delims) {
    std::vector<std::string> parts = {};
    boost::split(parts, s, boost::is_any_of(delims));
    return parts;
}

std::string StringUtil::trim_leading_whitespace(const std::string& s) {
    size_t first = s.find_first_not_of(' ');
    if (first == std::string::npos)
        return "";
    return s.substr(first);
}
