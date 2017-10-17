#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <sys/stat.h>
#include "FileUtil.h"

std::string FileUtil::expand_path(const std::string& filepath) {
    if (filepath.at(0) == '~') {
        std::string expanded = getenv("HOME");
        if (filepath.length() > 1) {
            expanded.append(filepath.substr(1));
        }
        return expanded;
    }
    return filepath;
}

bool FileUtil::file_exists(const std::string& name) {
    struct stat buffer;
    return (stat(name.c_str(), &buffer) == 0);
}

std::string FileUtil::get_contents(const std::string& filepath) {
    std::ifstream fin(filepath);
    return get_contents(fin);
}

std::string FileUtil::get_contents(const std::ifstream& fin) {
    std::stringstream sstr;
    sstr << fin.rdbuf();
    return sstr.str();
}

std::string FileUtil::get_extension(const std::string& name) {
    boost::filesystem::path path(name);
    std::string ext = path.extension().string();
    if (name == ext) {
        return "";
    }
    if (!ext.empty() && ext[0] == '.') {
        ext = ext.substr(1);
    }
    if (ext != "Z") {
        ext = boost::to_lower_copy(ext);
    }
    return ext;
}

bool FileUtil::is_directory(const std::string& name) {
    boost::filesystem::path path(name);
    return boost::filesystem::is_directory(path);
}

bool FileUtil::is_regular_file(const std::string& name) {
    boost::filesystem::path path(name);
    return boost::filesystem::is_regular_file(path);
}

bool FileUtil::is_dot_dir(const std::string& name) {
    return name == "." || name == "./" || name == ".." || name == "../";
    boost::filesystem::path path(name);
    return path.filename_is_dot() || path.filename_is_dot_dot();
}

bool FileUtil::is_hidden(const std::string& name) {
    boost::filesystem::path path(name);
    return !name.empty() && name.at(0) == '.' && !FileUtil::is_dot_dir(name);
}

std::vector<std::string> FileUtil::split_path(const std::string& filepath) {
    std::vector<std::string> parts;
    boost::filesystem::path path(filepath);
    if (FileUtil::is_dot_dir(filepath)) {
        if (path.filename_is_dot()) {
            parts.emplace_back(".");
        } else if (path.filename_is_dot_dot()) {
            parts.emplace_back("..");
        }
        return parts;
    }
    int i = 0;
    for(auto& part : path) {
        //cout << part << endl;
        if (part.string() == ".") {
            if (i == 0) {
                parts.push_back(part.string());
            }
        } else if (part.string() != "/") {
            parts.push_back(part.string());
        }
        i++;
    }
    return parts;
}
