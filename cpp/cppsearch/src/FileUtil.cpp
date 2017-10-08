#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <sys/stat.h>
#include <sstream>
#include "FileUtil.h"

bool FileUtil::file_exists(const std::string* name) {
    struct stat buffer;
    return (stat(name->c_str(), &buffer) == 0);
}

std::string FileUtil::get_contents(const std::string* filepath) {
    std::ifstream fin(*filepath);
    return get_contents(fin);
}

std::string FileUtil::get_contents(const std::ifstream& fin) {
    std::stringstream sstr;
    sstr << fin.rdbuf();
    return sstr.str();
}

std::string FileUtil::get_extension(const std::string* name) {
    boost::filesystem::path path(*name);
    std::string ext = path.extension().string();
    if (!ext.empty() && ext[0] == '.') {
        ext = ext.substr(1);
    }
    if (ext != "Z") {
        ext = boost::to_lower_copy(ext);
    }
    return ext;
}

bool FileUtil::is_directory(const std::string* name) {
    boost::filesystem::path path(*name);
    return boost::filesystem::is_directory(path);
}

bool FileUtil::is_regular_file(const std::string* name) {
    boost::filesystem::path path(*name);
    return boost::filesystem::is_regular_file(path);
}

bool FileUtil::is_dot_dir(const std::string* name) {
    boost::filesystem::path path(*name);
    return path.filename_is_dot() || path.filename_is_dot_dot();
}

bool FileUtil::is_hidden(const std::string* name) {
    boost::filesystem::path path(*name);
    return !name->empty() && name->substr(0,1) == "." && !FileUtil::is_dot_dir(name);
}

std::vector<std::string> FileUtil::split_path(const std::string& filepath) {
    std::vector<std::string> parts;
    for(auto& part : boost::filesystem::path(filepath)) {
        //cout << part << endl;
        if (part.string() != "/") {
            parts.push_back(part.string());
        }
    }
    return parts;
}
