xsearch
=======

This repo has the original python implementation as well as re-implementations
in various programming languages of a file search utility I created some time
ago. I wrote the other language versions as a way to help learn the basics of
those languages and compare their strengths and weaknesses.

The search utility has the same basic functionality across all of the languages
(although the python version is still the most full-featured at this point).
In most cases the code includes several types/classes:

* Searcher - executes the file search
* SearchSettings - encapsulates search settings, including directory or file
extensions to search or ignore, search patterns, etc.
* FileUtil - helps determine file type (e.g. binary vs. text), searchability,
etc.
* SearchResult - encapsulates a single search result

A main function allows for executing searches from the command line, as long
as a valid set of search settings-related arguments are included. A typical
usage string, this one for the csharp version CsSearch:

    Usage:
    CsSearch.exe [options] -s "<searchpattern>" <startdir>
    Options:
      -d "<dirname_pattern>"     Pattern for dirnames to include
      -D "<dirname_pattern>"     Pattern for dirnames to exclude
      -f "<filename_pattern>"    Pattern for filenames to include
      -F "<filename_pattern>"    Pattern for filenames to exclude
      --filelist                 Print list of matching files at end
      -t                         Time the execution
      -v                         Set verbose mode
      -x "<ext1>[,<ext2>]"       Extension(s) for files to include
      -X "<ext1>[,<ext2>]"       Extension(s) for files to exclude

For example, to find lines matching "\bLogin" in csharp files under the
current directory (recursively) that include "Controller" in the name but are
not in a directory that includes "temp" in its name, and you want to time the
execution and generate a list of matching files at the end, you would use a
command like this:

    CsSearch.exe -x cs -f "Controller" -D "temp" -s "\bLogin" -t --filelist .
