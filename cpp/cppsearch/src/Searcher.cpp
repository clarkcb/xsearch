#include <algorithm>
#include <boost/filesystem.hpp>
#include <iostream>
#include <common.h>
#include <FileTypes.h>
#include <FileUtil.h>
#include <SearchFile.h>
#include <SearchResult.h>
#include <SearchException.h>
#include "Searcher.h"

Searcher::Searcher(SearchSettings* ss) {
    validate_settings(ss);
    settings = ss;
    filetypes = new FileTypes();
}

void Searcher::validate_settings(SearchSettings* ss) {
    string* startpath = ss->get_startpath();
    if (startpath == nullptr || startpath->empty()) {
        throw SearchException("Startpath not defined");
    }
    string expanded = FileUtil::expand_path(*startpath);
    if (!FileUtil::file_exists(*startpath) && !FileUtil::file_exists(expanded)) {
        throw SearchException("Startpath not found");
    }
    if (ss->get_searchpatterns()->empty()) {
        throw SearchException("No search patterns defined");
    }
}

SearchFile* Searcher::get_searchfile(string& filepath) {
    FileType filetype = filetypes->get_filetype(filepath);
    boost::filesystem::path path(filepath);
    string parent_path = path.parent_path().string();
    string filename = path.filename().string();
    return new SearchFile(parent_path, filename, filetype);
}

vector<SearchResult*> Searcher::search() {
    string* startpath = settings->get_startpath();
    string expanded = FileUtil::expand_path(*startpath);
    if (FileUtil::is_directory(*startpath)) {
        return search_path(*startpath);

    } else if (FileUtil::is_directory(expanded)) {
        return search_path(expanded);

    } else if (FileUtil::is_regular_file(*startpath)) {
        auto* sf = get_searchfile(*startpath);
        return search_file(sf);

    } else if (FileUtil::is_regular_file(expanded)) {
        auto* sf = get_searchfile(expanded);
        return search_file(sf);

    } else {
        throw SearchException("startpath is an unsupported file type");
    }
}

bool matches_any_pattern(const string& s, vector<SearchPattern*> patterns) {
    smatch pmatch;
    for (auto& p : patterns) {
        if (regex_search(s, pmatch, p->r)) {
            return true;
        }
    }
    return false;
}

bool any_matches_any_pattern(vector<string> ss, const vector<SearchPattern*>& patterns) {
    for (auto& s : ss) {
        if (matches_any_pattern(s, patterns)) {
            return true;
        }
    }
    return false;
}

bool Searcher::is_search_dir(string filepath) {
    vector<string> elems = FileUtil::split_path(filepath);
    if (settings->get_excludehidden()) {
        for (auto& elem : elems) {
            if (FileUtil::is_hidden(elem)) {
                return false;
            }
        }
    }
    vector<SearchPattern*>* in_dirpatterns = settings->get_in_dirpatterns();
    vector<SearchPattern*>* out_dirpatterns = settings->get_out_dirpatterns();
    return ((in_dirpatterns->empty() || any_matches_any_pattern(elems, *in_dirpatterns))
            && (out_dirpatterns->empty() || !any_matches_any_pattern(elems, *out_dirpatterns)));
}

vector<string> Searcher::get_search_dirs(string filepath) {
    boost::filesystem::path p(filepath);
    vector<string> searchdirs = {filepath};

    vector<boost::filesystem::directory_entry> v;
    copy(boost::filesystem::directory_iterator(p), boost::filesystem::directory_iterator(), back_inserter(v));

    for (vector<boost::filesystem::directory_entry>::const_iterator it = v.begin(); it != v.end(); ++it) {
        boost::filesystem::path subpath = (*it).path();
        if (boost::filesystem::is_directory(subpath)) {
            if (is_search_dir(subpath.string())) {
                vector<string> subsearchdirs = get_search_dirs(subpath.string());
                if (!subsearchdirs.empty()) {
                    searchdirs.insert(searchdirs.end(), subsearchdirs.begin(), subsearchdirs.end());
                }
            }
        }
    }
    return searchdirs;
}

bool Searcher::is_search_file(string filename) {
    string ext = FileUtil::get_extension(filename);
    vector<string>* in_exts = settings->get_in_extensions();
    vector<string>* out_exts = settings->get_out_extensions();
    if ((!in_exts->empty() && find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
        || (!out_exts->empty() && find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
        return false;
    }
    vector<SearchPattern*>* in_filepatterns = settings->get_in_filepatterns();
    vector<SearchPattern*>* out_filepatterns = settings->get_out_filepatterns();
    return ((in_filepatterns->empty() || matches_any_pattern(filename, *in_filepatterns))
            && (out_filepatterns->empty() || !matches_any_pattern(filename, *out_filepatterns)));
}

bool Searcher::is_archive_search_file(string filename) {
    string ext = FileUtil::get_extension(filename);
    vector<string>* in_exts = settings->get_in_archiveextensions();
    vector<string>* out_exts = settings->get_out_archiveextensions();
    if ((!in_exts->empty() && find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
        || (!out_exts->empty() && find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
        return false;
    }
    vector<SearchPattern*>* in_filepatterns = settings->get_in_archivefilepatterns();
    vector<SearchPattern*>* out_filepatterns = settings->get_out_archivefilepatterns();
    return ((in_filepatterns->empty() || matches_any_pattern(filename, *in_filepatterns))
            && (out_filepatterns->empty() || !matches_any_pattern(filename, *out_filepatterns)));
}

bool Searcher::filter_file(string filepath) {
    boost::filesystem::path p(filepath);
    string filename = p.filename().string();
    if (FileUtil::is_hidden(filename) && settings->get_excludehidden()) {
        return false;
    }
    if (filetypes->get_filetype(filename) == FileType::ARCHIVE) {
        return settings->get_searcharchives() && is_archive_search_file(filename);
    }
    return !settings->get_archivesonly() && is_search_file(filename);
}

vector<SearchFile*> Searcher::get_search_files(vector<string> searchdirs) {
    vector<SearchFile*> searchfiles = {};

    vector<boost::filesystem::directory_entry> v;
    for (const auto& searchdir : searchdirs) {
        boost::filesystem::path p(searchdir);
        copy(boost::filesystem::directory_iterator(p), boost::filesystem::directory_iterator(), back_inserter(v));

        boost::filesystem::directory_iterator end_it;
        for (boost::filesystem::directory_iterator it(p); it != end_it; ++it) {
            boost::filesystem::path subpath = (*it).path();
            if (boost::filesystem::is_regular_file(subpath) && filter_file(subpath.string())) {
                string parent_path = subpath.parent_path().string();
                string filename = subpath.filename().string();
                FileType filetype = filetypes->get_filetype(filename);
                searchfiles.push_back(new SearchFile(parent_path, filename, filetype));
            }
        }
    }
    return searchfiles;
}

vector<SearchResult*> Searcher::search_path(string filepath) {
    vector<SearchResult*> results = {};
    vector<string> searchdirs;
    if (settings->get_recursive()) {
        searchdirs = get_search_dirs(filepath);
    } else {
        searchdirs = {filepath};
    }
    if (settings->get_verbose()) {
        string msg = "\nDirectories to be searched (";
        msg.append(to_string(searchdirs.size())).append("):");
        log(msg);
        for (const auto& searchdir : searchdirs) {
            log(searchdir);
        }
    }

    vector<SearchFile*> searchfiles = get_search_files(searchdirs);
    if (settings->get_verbose()) {
        string msg = "\nFiles to be searched (";
        msg.append(to_string(searchfiles.size())).append("):");
        log(msg);
        for (const auto& searchfile : searchfiles) {
            log(searchfile->searchfile_to_string());
        }
    }

    for (const auto& searchfile : searchfiles) {
        vector<SearchResult*> fileresults = search_file(searchfile);
        if (!fileresults.empty()) {
            results.insert(results.end(), fileresults.begin(), fileresults.end());
        }
    }
    return results;
}

vector<SearchResult*> Searcher::search_file(SearchFile* sf) {
    vector<SearchResult*> results = {};
    if (sf->filetype == FileType::CODE) {
        results = search_text_file(sf);
    } else if (sf->filetype == FileType::XML) {
        results = search_text_file(sf);
    } else if (sf->filetype == FileType::TEXT) {
        results = search_text_file(sf);
    } else if (sf->filetype == FileType::BINARY) {
        results = search_binary_file(sf);
    } else if (sf->filetype == FileType::ARCHIVE) {
        //cout << "searchfile is an ARCHIVE file: " << sf->filename << endl;
    } else {
        cout << "searchfile is an UNKNOWN file: " << sf->filename << endl;
    }
    return results;
}

vector<SearchResult*> Searcher::search_text_file(SearchFile* sf) {
    //cout << "Searching text file " << sf->searchfile_to_string() << endl;
    std::ifstream fin(sf->searchfile_to_string());
    vector<SearchResult*> results = search_ifstream(fin);
    fin.close();

    for (const auto& r : results) {
        r->searchfile = sf;
    }
    return results;
}

vector<SearchResult*> Searcher::search_ifstream(std::ifstream& fin) {
    if (settings->get_multilinesearch()) {
        return search_ifstream_contents(fin);
    } else {
        return search_ifstream_lines(fin);
    }
}

vector<SearchResult*> Searcher::search_ifstream_lines(std::ifstream& fin) {
    vector<SearchResult*> results = {};
    set<string> found_patterns = {};

    int linenum = 0;
    smatch pmatch;
    string line;

    deque<string> lines_before;
    deque<string> lines_after;
    unsigned int lines_before_count = settings->get_linesbefore();
    unsigned int lines_after_count = settings->get_linesafter();

    while (true) {
        ++linenum;
        if (!lines_after.empty()) {
            line = lines_after.front();
            lines_after.pop_front();
        } else if (getline(fin, line)) {
            // nothing to do, action in if clause
        } else {
            break;
        }

        if (lines_after_count > 0) {
            string next_line;
            while (lines_after.size() < lines_after_count && getline(fin, next_line)) {
                lines_after.push_back(next_line);
            }
        }

        for (const auto& p : *(settings->get_searchpatterns())) {
            if (settings->get_firstmatch() && found_patterns.find(*p->pattern) != found_patterns.end()) {
                continue;
            }
            unsigned long trimmed = 0;
            string linebuf = string(line);
            bool skip_pattern = false;
            while (!skip_pattern && !linebuf.empty() && regex_search(linebuf, pmatch, p->r)) {
                for (unsigned i=0; i < pmatch.size(); ++i) {
                    unsigned long match_start_idx = pmatch.position(i) + trimmed;
                    unsigned long match_end_idx = match_start_idx + pmatch.length(i);
                    vector<string>* v_lines_before = new vector<string>(lines_before.begin(), lines_before.end());
                    vector<string>* v_lines_after = new vector<string>(lines_after.begin(), lines_after.end());
                    results.push_back(new SearchResult(p, nullptr, linenum, match_start_idx + 1, match_end_idx + 1,
                                                       line, v_lines_before, v_lines_after));
                    if (settings->get_firstmatch()) {
                        found_patterns.insert(*p->pattern);
                        skip_pattern = true;
                        break;
                    }
                    linebuf = line.substr(match_end_idx);
                    trimmed = match_end_idx;
                }
            }
        }

        if (lines_before_count > 0) {
            if (lines_before.size() == lines_before_count) {
                lines_before.pop_front();
            }
            if (lines_before.size() < lines_before_count) {
                lines_before.push_back(line);
            }
        }
    }
    return results;
}

vector<SearchResult*> Searcher::search_ifstream_contents(std::ifstream& fin) {
    string contents = FileUtil::get_contents(fin);
    return search_string(&contents);
}

vector<unsigned long> get_newline_indices(string* s) {
    vector<unsigned long> newline_indices = {};
    for (unsigned long i=0; i < s->length(); i++) {
        if (s->at(i) == '\n') {
            newline_indices.push_back(i);
        }
    }
    return newline_indices;
}

long get_linenum_for_pos(vector<unsigned long> newline_indices, long pos) {
    long i = 0;
    while (newline_indices[i] < pos) i++;
    return ++i;
}

string get_line_for_pos(string* s, vector<unsigned long> newline_indices, long pos) {
    long i = 0;
    while (newline_indices[i] < pos) i++;
    unsigned long line_start_idx = newline_indices[i-1] + 1;
    unsigned long line_len = newline_indices[i] - newline_indices[i-1] - 1;
    return s->substr(line_start_idx, line_len);
}

vector<SearchResult*> Searcher::search_string(string* s) {
    vector<SearchResult*> results = {};
    set<string> found_patterns = {};
    vector<unsigned long> newline_indices = get_newline_indices(s);

    smatch pmatch;
    for (const auto& p : *(settings->get_searchpatterns())) {
        if (settings->get_firstmatch() && found_patterns.find(*p->pattern) != found_patterns.end()) {
            continue;
        }
        unsigned long trimmed = 0;
        string sbuf = string(*s);
        bool skip_pattern = false;
        while (!skip_pattern && !sbuf.empty() && regex_search(sbuf, pmatch, p->r)) {
            for (unsigned i=0; i < pmatch.size(); ++i) {
                unsigned long match_start_idx = pmatch.position(i) + trimmed;
                unsigned long match_end_idx = match_start_idx + pmatch.length(i);
                long linenum = get_linenum_for_pos(newline_indices, match_start_idx);
                string line = get_line_for_pos(s, newline_indices, match_start_idx);
                unsigned long line_match_start_idx = match_start_idx - newline_indices[linenum - 2];
                unsigned long line_match_end_idx = line_match_start_idx + pmatch.length();
                results.push_back(new SearchResult(p, nullptr, linenum, line_match_start_idx, line_match_end_idx, line));
                if (settings->get_firstmatch()) {
                    found_patterns.insert(*p->pattern);
                    skip_pattern = true;
                    break;
                }
                sbuf = s->substr(match_end_idx);
                trimmed = match_end_idx;
            }
        }
    }

    return results;
}

vector<SearchResult*> Searcher::search_binary_file(SearchFile* sf) {
    vector<SearchResult*> results = {};
    set<string> found_patterns = {};

    std::ifstream fin(sf->searchfile_to_string());
    string s = FileUtil::get_contents(fin);
    fin.close();

    smatch pmatch;
    for (const auto& p : *(settings->get_searchpatterns())) {
        if (settings->get_firstmatch() && found_patterns.find(*p->pattern) != found_patterns.end()) {
            continue;
        }
        unsigned long trimmed = 0;
        string sbuf = string(s);
        bool skip_pattern = false;
        while (!skip_pattern && !sbuf.empty() && regex_search(sbuf, pmatch, p->r)) {
            for (unsigned i=0; i < pmatch.size(); ++i) {
                unsigned long match_start_idx = pmatch.position(i) + trimmed;
                unsigned long match_end_idx = match_start_idx + pmatch.length(i);
                results.push_back(new SearchResult(p, sf, 0, match_start_idx, match_end_idx, ""));
                if (settings->get_firstmatch()) {
                    found_patterns.insert(*p->pattern);
                    skip_pattern = true;
                    break;
                }
                sbuf = s.substr(match_end_idx);
                trimmed = match_end_idx;
            }
        }
    }

    return results;
}

