#include <iostream>
#include <Searcher.h>
#include <StringUtil.h>
#include <SearchException.h>
#include "common.h"
#include "SearchOptions.h"

using namespace std;

vector<string> get_result_dirs(vector<SearchResult*>* results) {
    vector<string> result_dirs = {};
    for (const auto& result : *results) {
        if (find(result_dirs.begin(), result_dirs.end(), result->searchfile->path) == result_dirs.end())
        result_dirs.push_back(result->searchfile->path);
    }
    return result_dirs;
}

vector<string> get_result_files(vector<SearchResult*>* results) {
    vector<string> result_files = {};
    for (const auto& result : *results) {
        if (find(result_files.begin(), result_files.end(), result->searchfile->searchfile_to_string()) == result_files.end())
            result_files.push_back(result->searchfile->searchfile_to_string());
    }
    return result_files;
}

vector<string> get_result_lines(vector<SearchResult*>* results, bool unique) {
    vector<string> result_lines = {};
    for (const auto& result : *results) {
        if (!unique || find(result_lines.begin(), result_lines.end(), result->line) == result_lines.end())
            result_lines.push_back(StringUtil::trim_leading_whitespace(result->line));
    }
    sort(result_lines.begin(), result_lines.end());
    return result_lines;
}

int main(int argc, char *argv[]) {
    SearchOptions* options;

    try {
        options = new SearchOptions();
    } catch (const SearchException& e) {
        log("");
        log_error(e.what());
        exit(1);
    }

    try {
        SearchSettings* settings = options->settings_from_args(argc, argv);

        if (settings->get_debug()) {
            log(settings->settings_to_string());
        }

        if (settings->get_printusage()) {
            options->usage();
        }

        auto* searcher = new Searcher(settings);

        vector<SearchResult*> results = searcher->search();

        if (settings->get_printresults()) {
            string msg = "\nSearch results (";
            msg.append(to_string(results.size())).append("):");
            log(msg);
            for (const auto& result : results) {
                log(result->result_to_string());
            }
        }

        if (settings->get_listdirs()) {
            vector<string> result_dirs = get_result_dirs(&results);
            string msg = "\nDirectories with matches";
            if (result_dirs.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(to_string(result_dirs.size())).append("):");
                log(msg);
                for (const auto& d : result_dirs) {
                    log(d);
                }
            }
        }

        if (settings->get_listfiles()) {
            vector<string> result_files = get_result_files(&results);
            string msg = "\nFiles with matches";
            if (result_files.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(to_string(result_files.size())).append("):");
                log(msg);
                for (const auto& f : result_files) {
                    log(f);
                }
            }
        }

        if (settings->get_listlines()) {
            vector<string> result_lines = get_result_lines(&results, settings->get_uniquelines());
            string msg;
            if (settings->get_uniquelines()) {
                msg = "\nUnique lines with matches";
            } else {
                msg = "\nLines with matches";
            }
            if (result_lines.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(to_string(result_lines.size())).append("):");
                log(msg);
                for (const auto& l : result_lines) {
                    log(l);
                }
            }
        }
    } catch (const SearchException& e) {
        log("");
        log_error(e.what());
        options->usage();
    }

    return 0;
}
