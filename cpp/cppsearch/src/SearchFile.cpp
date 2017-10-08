#include <boost/filesystem.hpp>
#include "SearchFile.h"

using namespace std;

SearchFile::SearchFile(const string p, const string f, const FileType ft) {
    path = p;
    filename = f;
    filetype = ft;
}

string SearchFile::searchfile_to_string() {
    boost::filesystem::path p(path);
    boost::filesystem::path f(filename);
    p.append(filename);
    string fullpath = p.string();
    return fullpath;
}
