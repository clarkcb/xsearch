#ifndef CPPSEARCH_FILEUTIL_H
#define CPPSEARCH_FILEUTIL_H

#include <string>
#include <vector>

namespace cppsearch {
    class FileUtil {
    public:
        static std::string expand_path(const std::string& filepath);
        static bool file_exists(const std::string& name);
        static uint64_t file_size(const std::string& filepath);
        static std::string get_contents(const std::string& filepath);
        static std::string get_contents(const std::ifstream& fin);
        static std::string get_extension(const std::string& name);
        static std::string get_filename(const std::string& filepath);
        static bool is_directory(const std::string& name);
        static bool is_regular_file(const std::string& name);
        static bool is_dot_dir(const std::string& name);
        static bool is_hidden(const std::string& name);
        static std::string join_path(const std::string& path1, const std::string& path2);
        static std::vector<std::string> split_path(const std::string& filepath);

    private:
        // Disallow creating an instance of this object
        FileUtil() = default;
    };
}

#endif //CPPSEARCH_FILEUTIL_H
