#ifndef CPPSEARCH_SEARCHFILE_H
#define CPPSEARCH_SEARCHFILE_H

#include <string>
#include <vector>
#include "FileTypes.h"

namespace cppsearch {
    class SearchFile {
    private:
        const std::string CONTAINER_SEPARATOR = "!";
        std::vector<std::string> m_containers;
        std::string m_path;
        std::string m_file_name;
        FileType m_file_type;
        void init(const std::vector<std::string>& containers, const std::string& path, const std::string& file_name, FileType file_type);

    public:
        SearchFile(std::string& path, std::string& file_name, FileType file_type);
        SearchFile(const std::vector<std::string>& containers, std::string& path, std::string& file_name, FileType file_type);
        SearchFile(const std::vector<std::string> &containers, const std::string& path, const std::string& file_name, FileType file_type);
        [[nodiscard]] std::string path() const;
        [[nodiscard]] std::string file_name() const;
        FileType file_type();
        [[nodiscard]] const std::string string() const;
    };
}

#endif // CPPSEARCH_SEARCHFILE_H
