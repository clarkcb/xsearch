#ifndef CPPSEARCH_STRINGUTIL_H
#define CPPSEARCH_STRINGUTIL_H

#include <string>
#include <vector>

namespace cppsearch {
    class StringUtil {
    public:
        static std::vector<std::string> split_string(const std::string& s, const std::string& delims);
        static void ltrim(std::string& s);
        static std::string ltrim_copy(std::string s);
        static void rtrim(std::string& s);
        static std::string rtrim_copy(std::string s);
        static void trim(std::string& s);
        static std::string trim_copy(std::string s);

    private:
        // Disallow creating an instance of this object
        StringUtil() = default;
    };
}

#endif //CPPSEARCH_STRINGUTIL_H
