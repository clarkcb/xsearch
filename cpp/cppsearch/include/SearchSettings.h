#ifndef CPPSEARCH_SEARCHSETTINGS_H
#define CPPSEARCH_SEARCHSETTINGS_H

#include <cstdlib>
#include "FileTypes.h"
#include "SearchPattern.h"

using namespace std;

class SearchSettings {
private:
    bool archivesonly = false;
    bool debug = false;
    bool excludehidden = true;
    bool firstmatch = false;

    vector<string> in_archiveextensions;
    vector<SearchPattern*> in_archivefilepatterns;
    vector<SearchPattern*> in_dirpatterns;
    vector<string> in_extensions;
    vector<SearchPattern*> in_filepatterns;
    vector<const FileType*> in_filetypes;

    vector<SearchPattern*> in_linesafterpatterns;
    vector<SearchPattern*> in_linesbeforepatterns;

    vector<SearchPattern*> linesaftertopatterns;
    vector<SearchPattern*> linesafteruntilpatterns;

    unsigned int linesafter = 0;
    unsigned int linesbefore = 0;
    bool listdirs = false;
    bool listfiles = false;
    bool listlines = false;
    unsigned int maxlinelength = 0;
    bool multilinesearch = false;

    vector<string> out_archiveextensions;
    vector<SearchPattern*> out_archivefilepatterns;
    vector<SearchPattern*> out_dirpatterns;
    vector<string> out_extensions;
    vector<SearchPattern*> out_filepatterns;
    vector<const FileType*> out_filetypes;

    vector<SearchPattern*> out_linesafterpatterns;
    vector<SearchPattern*> out_linesbeforepatterns;

    bool printresults = false;
    bool printusage = false;
    bool printversion = false;
    bool recursive = true;
    bool searcharchives = false;

    vector<SearchPattern*> searchpatterns;
    string* startpath;

    bool uniquelines = false;
    bool verbose = false;

    string bool_to_string(bool b);
    string string_vector_to_string(vector<string>* s);
    string searchpatterns_to_string(vector<SearchPattern*>* ps);

    void add_pattern(const string* p, vector<SearchPattern*>* ps);
    void add_extensions(const string* exts, vector<string>* extensions);

public:
    SearchSettings();
    void add_in_archiveextension(const string* ext);
    void add_in_archivefilepattern(const string* pattern);
    void add_in_dirpattern(const string* pattern);
    void add_in_extension(const string* ext);
    void add_in_filepattern(const string* pattern);
    void add_in_filetype(const FileType* filetype);
    void add_in_linesafterpattern(const string* pattern);
    void add_in_linesbeforepattern(const string* pattern);
    void add_linesaftertopattern(const string* pattern);
    void add_linesafteruntilpattern(const string* pattern);
    void add_out_archiveextension(const string* ext);
    void add_out_archivefilepattern(const string* pattern);
    void add_out_dirpattern(const string* pattern);
    void add_out_extension(const string* ext);
    void add_out_filepattern(const string* pattern);
    void add_out_filetype(const FileType* filetype);
    void add_out_linesafterpattern(const string* pattern);
    void add_out_linesbeforepattern(const string* pattern);
    void add_searchpattern(const string* searchpattern);

    bool get_archivesonly();
    bool get_debug();
    bool get_excludehidden();
    bool get_firstmatch();
    bool get_multilinesearch();
    unsigned int get_linesafter();
    unsigned int get_linesbefore();
    bool get_listdirs();
    bool get_listfiles();
    bool get_listlines();
    unsigned int get_maxlinelength();
    bool get_printresults();
    bool get_printusage();
    bool get_printversion();
    bool get_recursive();
    bool get_searcharchives();
    string* get_startpath();
    bool get_uniquelines();
    bool get_verbose();

    vector<string>* get_in_archiveextensions();
    vector<SearchPattern*>* get_in_archivefilepatterns();
    vector<SearchPattern*>* get_in_dirpatterns();
    vector<string>* get_in_extensions();
    vector<SearchPattern*>* get_in_filepatterns();

    vector<string>* get_out_archiveextensions();
    vector<SearchPattern*>* get_out_archivefilepatterns();
    vector<SearchPattern*>* get_out_dirpatterns();
    vector<string>* get_out_extensions();
    vector<SearchPattern*>* get_out_filepatterns();
    vector<SearchPattern*>* get_searchpatterns();

    // bool is_in_archiveextension(const string* ext);
    // bool is_in_extension(const string* ext);
    // bool is_in_filetype(const FileType* filetype);
    // bool is_out_archiveextension(const string* ext);
    // bool is_out_extension(const string* ext);

    void set_archivesonly(bool b);
    void set_debug(bool b);
    void set_excludehidden(bool b);
    void set_firstmatch(bool b);
    void set_multilinesearch(bool b);
    void set_linesafter(unsigned int linecount);
    void set_linesbefore(unsigned int linecount);
    void set_listdirs(bool b);
    void set_listfiles(bool b);
    void set_listlines(bool b);
    void set_maxlinelength(unsigned int max);
    void set_printresults(bool b);
    void set_printusage(bool b);
    void set_printversion(bool b);
    void set_recursive(bool b);
    void set_searcharchives(bool b);
    void set_startpath(string* startpath);
    void set_uniquelines(bool b);
    void set_verbose(bool b);
    string settings_to_string();
};

#endif //CPPSEARCH_SEARCHSETTINGS_H
