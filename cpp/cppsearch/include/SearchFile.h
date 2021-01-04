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
        std::string m_filename;
        FileType m_filetype;
        void init(const std::vector<std::string>& containers, const std::string& path, const std::string& filename, FileType filetype);

    public:
        SearchFile(std::string& path, std::string& filename, FileType filetype);
        SearchFile(const std::vector<std::string>& containers, std::string& path, std::string& filename, FileType filetype);
        SearchFile(const std::vector<std::string> &containers, const std::string& path, const std::string& filename, FileType filetype);
        std::string path() const;
        std::string filename() const;
        FileType filetype();
        const std::string string() const;
    };
}

#endif // CPPSEARCH_SEARCHFILE_H
