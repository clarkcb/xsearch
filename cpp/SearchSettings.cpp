#include <cstdlib>
#include <iostream>
using namespace std;
#include "SearchSettings.h"

SearchSettings::SearchSettings()
{
    set<string> in_extensions();
    set<string> out_extensions();
    set<string> searchpatterns();
}

void SearchSettings::add_in_extension(string &ext)
{
    in_extensions.insert(ext);
}

void SearchSettings::add_out_extension(string ext)
{
    out_extensions.insert(ext);
}

void SearchSettings::add_searchpattern(string searchpattern)
{
    searchpatterns.insert(searchpattern);
}

void SearchSettings::set_startpath(string s)
{
    startpath = s;
}

//string& SearchSettings::to_string()
//{
//  return "SearchSettings instance";
//}

