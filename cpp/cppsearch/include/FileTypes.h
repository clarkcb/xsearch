#ifndef CPPSEARCH_FILETYPES_H
#define CPPSEARCH_FILETYPES_H

#include <cstdlib>
#include <set>

using namespace std;

enum class FileType {UNKNOWN, ARCHIVE, BINARY, CODE, TEXT, XML};

class FileTypes {
public:
    FileTypes();
    static FileType from_name(const string& name);
    FileType get_filetype(const string& filepath);
    bool is_archive_file(const string& filepath);
    bool is_binary_file(const string& filepath);
    bool is_code_file(const string& filepath);
    bool is_searchable_file(const string& filepath);
    bool is_text_file(const string& filepath);
    bool is_unknown_file(const string& filepath);
    bool is_xml_file(const string& filepath);

private:
    set<string> archive_extensions;
    set<string> binary_extensions;
    set<string> code_extensions;
    set<string> text_extensions;
    set<string> xml_extensions;
    void load_filetypes();
    bool found_ext(const set<string>* ext_set, const string& ext);
};

#endif //CPPSEARCH_FILETYPES_H
