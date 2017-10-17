#include "boost/algorithm/string.hpp"
#include <FileUtil.h>
#include "rapidjson/document.h"
#include "rapidjson/filereadstream.h"
#include "common.h"
#include "config.h"
#include "FileTypes.h"

using namespace rapidjson;
using namespace std;

FileTypes::FileTypes() {
    archive_extensions = {};
    binary_extensions = {};
    text_extensions = {};
    load_filetypes();
}

void FileTypes::load_filetypes() {
    auto filetypes_path = string(XSEARCHPATH);
    filetypes_path.append("/shared/filetypes.json");

    if (!FileUtil::file_exists(filetypes_path)) {
        string msg = "Filetypes file not found: ";
        msg.append(filetypes_path);
        log_error(msg);
        // TODO: SearchException
        return;
    }

    FILE* fp = fopen(filetypes_path.c_str(), "r");

    char readBuffer[65536];
    FileReadStream is(fp, readBuffer, sizeof(readBuffer));

    Document document;

    document.ParseStream(is);
    fclose(fp);

    assert(document.HasMember("filetypes"));
    const Value& filetypes = document["filetypes"];
    assert(filetypes.IsArray());
    for (SizeType i = 0; i < filetypes.Size(); i++) {
        const Value::ConstObject &filetype = filetypes[i].GetObject();
        assert(filetype.HasMember("type"));
        const Value &typeValue = filetype["type"];
        string type = typeValue.GetString();

        assert(filetype.HasMember("extensions"));
        const Value& extensions = filetype["extensions"];

        for (SizeType j = 0; j < extensions.Size(); j++) {
            if (type == "archive") {
                archive_extensions.insert(extensions[j].GetString());
            } else if (type == "binary") {
                binary_extensions.insert(extensions[j].GetString());
            } else if (type == "code") {
                code_extensions.insert(extensions[j].GetString());
            } else if (type == "text") {
                text_extensions.insert(extensions[j].GetString());
            } else if (type == "xml") {
                xml_extensions.insert(extensions[j].GetString());
            }
        }
    }
}

FileType FileTypes::from_name(const string& name) {
    string uname = boost::to_upper_copy(name);
    if (uname == "TEXT") {
        return FileType::TEXT;
    }
    if (uname == "BINARY") {
        return FileType::BINARY;
    }
    if (uname == "CODE") {
        return FileType::CODE;
    }
    if (uname == "XML") {
        return FileType::XML;
    }
    if (uname == "ARCHIVE") {
        return FileType::ARCHIVE;
    }
    return FileType::UNKNOWN;
}

FileType FileTypes::get_filetype(const string& filepath) {
    if (is_code_file(filepath)) {
        return FileType::CODE;
    }
    if (is_xml_file(filepath)) {
        return FileType::XML;
    }
    if (is_text_file(filepath)) {
        return FileType::TEXT;
    }
    if (is_binary_file(filepath)) {
        return FileType::BINARY;
    }
    if (is_archive_file(filepath)) {
        return FileType::ARCHIVE;
    }
    return FileType::UNKNOWN;
}

bool FileTypes::found_ext(const set<string>* ext_set, const string& ext) {
    auto found = ext_set->find(ext);
    return found != ext_set->end();
}

bool FileTypes::is_archive_file(const string& filepath) {
    string ext = FileUtil::get_extension(filepath);
    return found_ext(&archive_extensions, ext);
}

bool FileTypes::is_binary_file(const string& filepath) {
    string ext = FileUtil::get_extension(filepath);
    return found_ext(&binary_extensions, ext);
}

bool FileTypes::is_code_file(const string& filepath) {
    string ext = FileUtil::get_extension(filepath);
    return found_ext(&code_extensions, ext);
}

bool FileTypes::is_searchable_file(const string& filepath) {
    string ext = FileUtil::get_extension(filepath);
    return found_ext(&text_extensions, ext)
           || found_ext(&code_extensions, ext)
           || found_ext(&xml_extensions, ext)
           || found_ext(&binary_extensions, ext)
           || found_ext(&archive_extensions, ext);
}

bool FileTypes::is_text_file(const string& filepath) {
    string ext = FileUtil::get_extension(filepath);
    return found_ext(&text_extensions, ext)
           || found_ext(&code_extensions, ext)
           || found_ext(&xml_extensions, ext);
}

bool FileTypes::is_unknown_file(const string& filepath) {
    return get_filetype(filepath) == FileType::UNKNOWN;
}

bool FileTypes::is_xml_file(const string& filepath) {
    string ext = FileUtil::get_extension(filepath);
    return found_ext(&xml_extensions, ext);
}
