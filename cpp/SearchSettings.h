#ifndef SEARCHSETTINGS_H
#define SEARCHSETTINGS_H

#include <cstdlib>
#include <set>
using namespace std;
class SearchSettings
{
public:
    SearchSettings();
    void add_in_extension(string &ext);
    void add_out_extension(string ext);
    void add_searchpattern(string searchpattern);
    void set_startpath(string startpath);
    string& to_string();

private:
    set<string> in_extensions;
    set<string> out_extensions;
    set<string> searchpatterns;
    string startpath;
};

#endif
