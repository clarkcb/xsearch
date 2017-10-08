#ifndef CPPSEARCH_SEARCHFILE_H
#define CPPSEARCH_SEARCHFILE_H

#include <string>
#include <vector>
#include "FileTypes.h"

using namespace std;

class SearchFile {
private:
    const string CONTAINER_SEPARATOR = "!";
    void init(const vector<string>& containers, const string& path, const string& filename, FileType filetype);

public:
    vector<string> containers;
    string path;
    string filename;
    FileType filetype;
    SearchFile(string& path, string& filename, FileType filetype);
    SearchFile(vector<string> containers, string& path, string& filename, FileType filetype);
    SearchFile(const vector<string> &containers, const string& path, const string& filename, FileType filetype);
    string searchfile_to_string();
};

#endif // CPPSEARCH_SEARCHFILE_H
