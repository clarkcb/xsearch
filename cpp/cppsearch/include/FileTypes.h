#ifndef CPPSEARCH_FILETYPES_H
#define CPPSEARCH_FILETYPES_H

#include <cstdlib>
#include <set>

namespace cppsearch {
    enum class FileType {UNKNOWN, ARCHIVE, BINARY, CODE, TEXT, XML};

    class FileTypes {
    public:
        FileTypes();
        static FileType from_name(const std::string& name);
        FileType get_file_type(const std::string& file_path);
        bool is_archive_file(const std::string& file_path);
        bool is_binary_file(const std::string& file_path);
        bool is_code_file(const std::string& file_path);
        bool is_searchable_file(const std::string& file_path);
        bool is_text_file(const std::string& file_path);
        bool is_unknown_file(const std::string& file_path);
        bool is_xml_file(const std::string& file_path);

    private:
        std::set<std::string> m_archive_extensions;
        std::set<std::string> m_binary_extensions;
        std::set<std::string> m_code_extensions;
        std::set<std::string> m_text_extensions;
        std::set<std::string> m_xml_extensions;
        void load_file_types();
        static bool found_ext(const std::set<std::string>* ext_set, const std::string& ext);
    };
}

#endif //CPPSEARCH_FILETYPES_H
