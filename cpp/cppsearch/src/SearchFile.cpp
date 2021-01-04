#include <boost/filesystem.hpp>
#include "SearchFile.h"

namespace cppsearch {
    SearchFile::SearchFile(std::string& p, std::string& f, FileType ft) {
        std::vector<std::string> containers = {};
        init(containers, p, f, ft);
    }

    SearchFile::SearchFile(const std::vector<std::string>& cs, const std::string& p, const std::string& f, const FileType ft) {
        init(cs, p, f, ft);
    }

    SearchFile::SearchFile(const std::vector<std::string>& cs, std::string& p, std::string& f, FileType ft) {
        init(cs, p, f, ft);
    }


    void SearchFile::init(const std::vector<std::string>& cs, const std::string& p, const std::string& f,
                          const FileType ft) {
        m_containers = cs;
        m_path = p;
        m_filename = f;
        m_filetype = ft;
    }

    std::string SearchFile::path() const {
        return m_path;
    }

    std::string SearchFile::filename() const {
        return m_filename;
    }

    FileType SearchFile::filetype() {
        return m_filetype;
    }

    const std::string SearchFile::string() const {
        std::string fullpath;
        for (const auto& c : m_containers) {
            fullpath.append(c).append(CONTAINER_SEPARATOR);
        }
        boost::filesystem::path p(m_path);
        p.append(m_filename);
        fullpath.append(p.string());
        return fullpath;
    }
}
