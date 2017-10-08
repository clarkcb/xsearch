#include <boost/filesystem.hpp>
#include "SearchFile.h"

using namespace std;

SearchFile::SearchFile(string& p, string& f, FileType ft) {
    vector<string> containers = {};
    init(containers, p, f, ft);
}

SearchFile::SearchFile(const vector<string>& cs, const string& p, const string& f, const FileType ft) {
    init(cs, p, f, ft);
}

SearchFile::SearchFile(vector<string> cs, string& p, string& f, FileType ft) {
    init(cs, p, f, ft);
}


void SearchFile::init(const vector<string>& cs, const string& p, const string& f,
                      const FileType ft) {
    containers = cs;
    path = p;
    filename = f;
    filetype = ft;
}

string SearchFile::searchfile_to_string() {
    string fullpath;
    for (const auto& c : containers) {
        fullpath.append(c).append(CONTAINER_SEPARATOR);
    }
    boost::filesystem::path p(path);
    boost::filesystem::path f(filename);
    p.append(filename);
    fullpath.append(p.string());
    return fullpath;
}
