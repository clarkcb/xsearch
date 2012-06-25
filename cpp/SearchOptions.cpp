#include <cstdlib>
#include <deque>
#include <iostream>
using namespace std;
#include "SearchOption.h"
#include "SearchOptions.h"
#include "SearchSettings.h"

SearchOptions::SearchOptions()
{
    SearchOption* arg_options[] = {
        new SearchOption("d", "dirname", "Specify name pattern for directories to include in search"),
        new SearchOption("D", "dirfilter", "Specify name pattern for directories to exclude from search"),
        new SearchOption("f", "filename", "Specify name pattern for files to include in search"),
        new SearchOption("F", "filefilter", "Specify name pattern for files to exclude from search"),
        new SearchOption("s", "search", "Specify search pattern"),
        new SearchOption("x", "ext", "Specify extension for files to include in search"),
        new SearchOption("X", "extfilter", "Specify extension for files to exclude from search"),
    };
    SearchOption* flag_options[] = {
        new SearchOption("1", "firstmatch", "Capture only the first match for a file+search combination"),
        new SearchOption("a", "allmatches", "Capture all matches*"),
        new SearchOption("", "debug", "Set output mode to debug"),
        new SearchOption("h", "help", "Print usage and exit"),
        new SearchOption("", "listfiles", "Generate a list of the matching files after searching"),
        new SearchOption("", "listlines", "Generate a list of the matching lines after searching"),
        new SearchOption("p", "printmatches", "Print matches to stdout as found*"),
        new SearchOption("P", "noprintmatches", "Suppress printing of matches to stdout"),
        new SearchOption("t", "dotiming", "Time search execution"),
        new SearchOption("v", "verbose", "Set output mode to verbose"),
        new SearchOption("V", "version", "Print version and exit"),
    };
}

void SearchOptions::usage()
{
    cout << "Missing options" << endl;
    exit(1);
}

void SearchOptions::searchsettings_from_args(int &argc, char *argv[], SearchSettings &settings)
{
    deque<string> arg_deque;
    unsigned int i;

    for (i=1; i < argc; i++)
    {
        arg_deque.push_back(argv[i]);
    }

    string next_arg;
    while (!arg_deque.empty())
    {
        next_arg = arg_deque.front();
        cout << "next_arg: " << next_arg << endl;
        arg_deque.pop_front();
        
        if (next_arg[0] == '-')
        {
            cout << "next_arg is a option" << endl;
            while (!next_arg.empty() && next_arg[0] == '-')
            {
                next_arg = next_arg.substr(1);
            }
            cout << "next_arg after trim: " << next_arg << endl;
            //TODO: check to see if next_arg is in arg_map or flag_map
            if (next_arg == "x" || next_arg == "ext")
            {
                if (arg_deque.empty())
                {
                    cout << "ERROR: missing value" << endl;
                    usage();
                }
                else
                {
                    string val;
                    val = arg_deque.front();
                    arg_deque.pop_front();
                    cout << "val: " << val << endl;
                    //settings.add_in_extension(val);
                }
            }
        }
    }
}

