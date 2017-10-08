#ifndef CPPSEARCH_SEARCHFILE_H
#define CPPSEARCH_SEARCHFILE_H

#include <string>
#include "FileTypes.h"

using namespace std;

class SearchFile {
public:
    string path;
    string filename;
    FileType filetype;
    SearchFile(string path, string filename, FileType filetype);
    string searchfile_to_string();
};

#endif // CPPSEARCH_SEARCHFILE_H
