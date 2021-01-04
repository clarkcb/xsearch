#include <boost/algorithm/string.hpp>
#include "StringUtil.h"

namespace cppsearch {
    std::vector<std::string> StringUtil::split_string(const std::string& s, const std::string& delims) {
        std::vector<std::string> parts = {};
        boost::split(parts, s, boost::is_any_of(delims));
        return parts;
    }

    // trim from start (in place)
    void StringUtil::ltrim(std::string &s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
            return !std::isspace(ch);
        }));
    }

    std::string StringUtil::ltrim_copy(std::string s) {
        StringUtil::ltrim(s);
        return s;
    }

    // trim from end (in place)
    void StringUtil::rtrim(std::string &s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
            return !std::isspace(ch);
        }).base(), s.end());
    }

    std::string StringUtil::rtrim_copy(std::string s) {
        StringUtil::rtrim(s);
        return s;
    }

    // trim from both ends (in place)
    void StringUtil::trim(std::string &s) {
        ltrim(s);
        rtrim(s);
    }

    std::string StringUtil::trim_copy(std::string s) {
        StringUtil::trim(s);
        return s;
    }
}
