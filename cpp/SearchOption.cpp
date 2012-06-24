#include <iostream>
#include <cstdlib>
using namespace std;
#include "SearchOption.h"

SearchOption::SearchOption(string sa, string la, string desc)
{
    shortarg = sa;
    longarg = la;
    description = desc;
    cout << "SearchOption("
        << "shortarg=\"" << shortarg << "\""
        << ", longarg=\"" << longarg << "\""
        << ", description=\"" << description << "\""
        << ")" << endl;
}
