#include <FileUtil.h>
#include <StringUtil.h>

using namespace std;
#include "SearchSettings.h"

SearchSettings::SearchSettings() {
    in_archiveextensions = {};
    in_archivefilepatterns = {};
    in_dirpatterns = {};
    in_extensions = {};
    in_filepatterns = {};
    in_filetypes = {};
    out_archiveextensions = {};
    out_archivefilepatterns = {};
    out_dirpatterns = {};
    out_extensions = {};
    out_filepatterns = {};
    out_filetypes = {};
    searchpatterns = {};
}

void SearchSettings::add_pattern(const string* p, vector<SearchPattern*>* ps) {
    ps->push_back(new SearchPattern(p));
}

void SearchSettings::add_extensions(const string* exts, vector<string>* extensions) {
    vector<string> xs = StringUtil::split_string(*exts, ",");
    extensions->insert(extensions->end(), xs.begin(), xs.end());
}

void SearchSettings::add_in_archiveextension(const string* ext) {
    add_extensions(ext, &in_archiveextensions);
}

void SearchSettings::add_in_archivefilepattern(const string* p) {
    add_pattern(p, &in_archivefilepatterns);
}

void SearchSettings::add_in_dirpattern(const string* p) {
    add_pattern(p, &in_dirpatterns);
}

void SearchSettings::add_in_extension(const string* ext) {
    add_extensions(ext, &in_extensions);
}

void SearchSettings::add_in_filepattern(const string* p) {
    add_pattern(p, &in_filepatterns);
}

void SearchSettings::add_in_filetype(const FileType* filetype) {
    in_filetypes.push_back(filetype);
}

void SearchSettings::add_in_linesafterpattern(const string* p) {
    add_pattern(p, &in_linesafterpatterns);
}

void SearchSettings::add_in_linesbeforepattern(const string* p) {
    add_pattern(p, &in_linesbeforepatterns);
}

void SearchSettings::add_linesaftertopattern(const string* p) {
    add_pattern(p, &linesaftertopatterns);
}

void SearchSettings::add_linesafteruntilpattern(const string* p) {
    add_pattern(p, &linesafteruntilpatterns);
}

void SearchSettings::add_out_archiveextension(const string* ext) {
    add_extensions(ext, &out_archiveextensions);
}

void SearchSettings::add_out_archivefilepattern(const string* p) {
    add_pattern(p, &out_archivefilepatterns);
}

void SearchSettings::add_out_dirpattern(const string* p) {
    add_pattern(p, &out_dirpatterns);
}

void SearchSettings::add_out_extension(const string* ext) {
    add_extensions(ext, &out_extensions);
}

void SearchSettings::add_out_filepattern(const string* p) {
    add_pattern(p, &out_filepatterns);
}

void SearchSettings::add_out_filetype(const FileType* filetype) {
    out_filetypes.push_back(filetype);
}

void SearchSettings::add_out_linesafterpattern(const string* p) {
    add_pattern(p, &out_linesafterpatterns);
}

void SearchSettings::add_out_linesbeforepattern(const string* p) {
    add_pattern(p, &out_linesbeforepatterns);
}

void SearchSettings::add_searchpattern(const string* p) {
    add_pattern(p, &searchpatterns);
}

bool SearchSettings::get_archivesonly() {
    return archivesonly;
}

bool SearchSettings::get_debug() {
    return debug;
}

bool SearchSettings::get_excludehidden() {
    return excludehidden;
}

bool SearchSettings::get_firstmatch() {
    return firstmatch;
}

bool SearchSettings::get_multilinesearch() {
    return multilinesearch;
}

unsigned int SearchSettings::get_linesafter() {
    return linesafter;
}

unsigned int SearchSettings::get_linesbefore() {
    return linesbefore;
}

bool SearchSettings::get_listdirs() {
    return listdirs;
}

bool SearchSettings::get_listfiles() {
    return listfiles;
}

bool SearchSettings::get_listlines() {
    return listlines;
}

unsigned int SearchSettings::get_maxlinelength() {
    return maxlinelength;
}

bool SearchSettings::get_printresults() {
    return printresults;
}

bool SearchSettings::get_printusage() {
    return printusage;
}

bool SearchSettings::get_printversion() {
    return printversion;
}

bool SearchSettings::get_recursive() {
    return recursive;
}

bool SearchSettings::get_searcharchives() {
    return searcharchives;
}

vector<string>* SearchSettings::get_in_archiveextensions() {
    return &in_archiveextensions;
}

vector<SearchPattern*>* SearchSettings::get_in_archivefilepatterns() {
    return &in_archivefilepatterns;
}

vector<SearchPattern*>* SearchSettings::get_in_dirpatterns() {
    return &in_dirpatterns;
}

vector<string>* SearchSettings::get_in_extensions() {
    return &in_extensions;
}

vector<SearchPattern*>* SearchSettings::get_in_filepatterns() {
    return &in_filepatterns;
}

vector<string>* SearchSettings::get_out_archiveextensions() {
    return &out_archiveextensions;
}

vector<SearchPattern*>* SearchSettings::get_out_archivefilepatterns() {
    return &out_archivefilepatterns;
}

vector<SearchPattern*>* SearchSettings::get_out_dirpatterns() {
    return &out_dirpatterns;
}

vector<string>* SearchSettings::get_out_extensions() {
    return &out_extensions;
}

vector<SearchPattern*>* SearchSettings::get_out_filepatterns() {
    return &out_filepatterns;
}

vector<SearchPattern*>* SearchSettings::get_searchpatterns() {
    return &searchpatterns;
}


string* SearchSettings::get_startpath() {
    return startpath;
}

bool SearchSettings::get_uniquelines() {
    return uniquelines;
}

bool SearchSettings::get_verbose() {
    return verbose;
}

void SearchSettings::set_archivesonly(const bool b) {
    archivesonly = b;
    if (b) searcharchives = b;
}

void SearchSettings::set_debug(const bool b) {
    debug = b;
    if (b) verbose = b;
}

void SearchSettings::set_excludehidden(const bool b) {
    excludehidden = b;
}

void SearchSettings::set_firstmatch(const bool b) {
    firstmatch = b;
}

void SearchSettings::set_linesafter(const unsigned int linecount) {
    linesafter = linecount;
}

void SearchSettings::set_linesbefore(const unsigned int linecount) {
    linesbefore = linecount;
}

void SearchSettings::set_listdirs(const bool b) {
    listdirs = b;
}

void SearchSettings::set_listfiles(const bool b) {
    listfiles = b;
}

void SearchSettings::set_listlines(const bool b) {
    listlines = b;
}

void SearchSettings::set_maxlinelength(const unsigned int max) {
    maxlinelength = max;
}

void SearchSettings::set_multilinesearch(const bool b) {
    multilinesearch = b;
}

void SearchSettings::set_printresults(const bool b) {
    printresults = b;
}

void SearchSettings::set_printusage(const bool b) {
    printusage = b;
}

void SearchSettings::set_printversion(const bool b) {
    printversion = b;
}

void SearchSettings::set_recursive(const bool b) {
    recursive = b;
}

void SearchSettings::set_searcharchives(const bool b) {
    searcharchives = b;
}

void SearchSettings::set_startpath(string* s) {
    startpath = s;
}

void SearchSettings::set_uniquelines(const bool b) {
    uniquelines = b;
}

void SearchSettings::set_verbose(const bool b) {
    verbose = b;
}

string SearchSettings::bool_to_string(bool b) {
    return b ? "true" : "false";
}

string SearchSettings::string_vector_to_string(vector<string>* ss) {
    string ss_string = "[";
    int count = 0;
    for (auto const& s : *ss) {
        if (count > 0) {
            ss_string.append(", ");
        }
        ss_string.append("\"").append(s).append("\"");
        count++;
    }
    ss_string.append("]");
    return ss_string;
}

string SearchSettings::searchpatterns_to_string(vector<SearchPattern*>* ps) {
    string ps_string = "[";
    int count = 0;
    for (auto const& p : *ps) {
        if (count > 0) {
            ps_string.append(", ");
        }
        const string* pattern = p->pattern;
        ps_string.append("\"").append(*pattern).append("\"");
        count++;
    }
    ps_string.append("]");
    return ps_string;
}

string SearchSettings::settings_to_string() {
    string path = "\"\"";
    if (startpath != nullptr) {
        path = string("\"") + *startpath + "\"";
    }
    string settings_str =
            string("SearchSettings(")
            + "archivesonly: " + bool_to_string(archivesonly)
            + ", debug: " + bool_to_string(debug)
            + ", excludehidden: " + bool_to_string(excludehidden)
            + ", firstmatch: " + bool_to_string(firstmatch)
            + ", in_archiveextensions: " + string_vector_to_string(&in_archiveextensions)
            + ", in_archivefilepatterns: " + searchpatterns_to_string(&in_archivefilepatterns)
            + ", in_dirpatterns: " + searchpatterns_to_string(&in_dirpatterns)
            + ", in_extensions: " + string_vector_to_string(&in_extensions)
            + ", in_filepatterns: " + searchpatterns_to_string(&in_filepatterns)
            + ", in_linesafterpatterns: " + searchpatterns_to_string(&in_linesafterpatterns)
            + ", in_linesbeforepatterns: " + searchpatterns_to_string(&in_linesbeforepatterns)
            + ", linesafter: " + to_string(linesafter)
            + ", linesaftertopatterns: " + searchpatterns_to_string(&linesaftertopatterns)
            + ", linesafteruntilpatterns: " + searchpatterns_to_string(&linesafteruntilpatterns)
            + ", linesbefore: " + to_string(linesbefore)
            + ", listdirs: " + bool_to_string(listdirs)
            + ", listfiles: " + bool_to_string(listfiles)
            + ", listlines: " + bool_to_string(listlines)
            + ", maxlinelength: " + to_string(maxlinelength)
            + ", multilinesearch: " + bool_to_string(multilinesearch)
            + ", out_archiveextensions: " + string_vector_to_string(&out_archiveextensions)
            + ", out_archivefilepatterns: " + searchpatterns_to_string(&out_archivefilepatterns)
            + ", out_dirpatterns: " + searchpatterns_to_string(&out_dirpatterns)
            + ", out_extensions: " + string_vector_to_string(&out_extensions)
            + ", out_filepatterns: " + searchpatterns_to_string(&out_filepatterns)
            + ", out_linesafterpatterns: " + searchpatterns_to_string(&out_linesafterpatterns)
            + ", out_linesbeforepatterns: " + searchpatterns_to_string(&out_linesbeforepatterns)
            + ", printresults: " + bool_to_string(printresults)
            + ", printusage: " + bool_to_string(printusage)
            + ", printversion: " + bool_to_string(printversion)
            + ", recursive: " + bool_to_string(recursive)
            + ", searcharchives: " + bool_to_string(searcharchives)
            + ", searchpatterns: " + searchpatterns_to_string(&searchpatterns)
            + ", startpath: " + path
            + ", uniquelines: " + bool_to_string(uniquelines)
            + ", verbose: " + bool_to_string(verbose)
            + ")";
    return settings_str;
}

std::ostream& operator<<(std::ostream& strm, SearchSettings& settings) {
    string settings_string = settings.settings_to_string();
    return strm << settings_string;
}
