#include "Searcher.h"
#include "SearchException.h"
#include "SearchResultFormatter.h"
#include "SearchOptions.h"
#include "cppfind.h"

using namespace cppsearch;

std::vector<std::string> get_result_dirs(std::vector<SearchResult*>* results) {
    std::set<std::string> result_dir_set = {};
    for (const auto& result : *results) {
        result_dir_set.insert(result->file()->path());
    }
    std::vector<std::string> result_dirs(result_dir_set.begin(), result_dir_set.end());
    return result_dirs;
}

std::vector<std::string> get_result_files(std::vector<SearchResult*>* results) {
    std::set<std::string> result_file_set = {};
    for (const auto& result : *results) {
        result_file_set.insert(result->file()->string());
    }
    std::vector<std::string> result_files(result_file_set.begin(), result_file_set.end());
    return result_files;
}

std::vector<std::string> get_result_lines(std::vector<SearchResult*>* results, bool unique) {
    std::set<std::string> result_line_set = {};
    for (const auto& result : *results) {
        result_line_set.insert(result->line());
    }
    std::vector<std::string> result_lines(result_line_set.begin(), result_line_set.end());
    sort(result_lines.begin(), result_lines.end());
    return result_lines;
}

int main(int argc, char *argv[]) {
    SearchOptions* options;

    try {
        options = new SearchOptions();
    } catch (const SearchException& e) {
        cppfind::log("");
        cppfind::log_error(e.msg());
        exit(1);
    }

    try {
        auto* settings = options->settings_from_args(argc, argv);

        if (settings->debug()) {
            cppfind::log(settings->string());
        }

        if (settings->print_usage()) {
            options->usage();
        }

        auto* searcher = new Searcher(settings);

        std::vector<SearchResult*> results = searcher->search();

        if (settings->print_results()) {
            auto* formatter = new SearchResultFormatter(settings);
            std::string msg = "\nSearch results (";
            msg.append(std::to_string(results.size())).append("):");
            cppfind::log(msg);
            for (const auto& result : results) {
                cppfind::log(formatter->format(result));
            }
        }

        if (settings->list_dirs()) {
            std::vector<std::string> result_dirs = get_result_dirs(&results);
            std::string msg = "\nDirectories with matches";
            if (result_dirs.empty()) {
                msg.append(": 0");
                cppfind::log(msg);
            } else {
                msg.append(" (").append(std::to_string(result_dirs.size())).append("):");
                cppfind::log(msg);
                for (const auto& d : result_dirs) {
                    cppfind::log(d);
                }
            }
        }

        if (settings->list_files()) {
            std::vector<std::string> result_files = get_result_files(&results);
            std::string msg = "\nFiles with matches";
            if (result_files.empty()) {
                msg.append(": 0");
                cppfind::log(msg);
            } else {
                msg.append(" (").append(std::to_string(result_files.size())).append("):");
                cppfind::log(msg);
                for (const auto& f : result_files) {
                    cppfind::log(f);
                }
            }
        }

        if (settings->list_lines()) {
            std::vector<std::string> result_lines = get_result_lines(&results, settings->unique_lines());
            std::string msg;
            if (settings->unique_lines()) {
                msg = "\nUnique lines with matches";
            } else {
                msg = "\nLines with matches";
            }
            if (result_lines.empty()) {
                msg.append(": 0");
                cppfind::log(msg);
            } else {
                msg.append(" (").append(std::to_string(result_lines.size())).append("):");
                cppfind::log(msg);
                for (const auto& l : result_lines) {
                    cppfind::log(l);
                }
            }
        }
    } catch (const SearchException& e) {
        cppfind::log("");
        cppfind::log_error(e.msg());
        options->usage();
    }

    return 0;
}
