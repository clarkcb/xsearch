#include <iostream>
#include <cstdlib>
using namespace std;
#include "SearchOptions.h"
#include "SearchSettings.h"

void usage()
{
    cout << "Missing options" << endl;
    exit(1);
}

//ostream& operator<<(ostream& out, const SearchSettings& s)
//{
//  return out<< s.toString();
//}

int main(int argc, char *argv[])
{
    SearchOptions *search_options = new SearchOptions();

    if (argc < 4)
    {
        search_options->usage();
    }

    SearchSettings *settings;
    search_options->searchsettings_from_args(argc, argv, *settings);
    return 0;
}
