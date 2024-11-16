#include <memory>
#include <set>
#include "Searcher.h"
#include "SearchException.h"
#include "SearchResultFormatter.h"
#include "SearchOptions.h"
#include "cppfind.h"

using namespace cppsearch;

std::vector<std::string> get_matching_dirs(const std::vector<SearchFileResult>& search_results) {
    std::unordered_set<std::string> dir_set;
    std::vector<std::string> matching_dirs;
    std::set<std::string> result_dir_set;
    for (const auto& sr : search_results) {
        const std::string dir = sr.file().file_path().parent_path().string();
        if (!dir_set.contains(dir)) {
            matching_dirs.push_back(dir);
        }
        dir_set.emplace(dir);
    }
    return matching_dirs;
}

std::vector<std::string> get_matching_files(const std::vector<SearchFileResult>& search_results) {
    std::unordered_set<std::string> file_set;
    std::vector<std::string> matching_files;
    matching_files.reserve(search_results.size());
    for (const auto& sr : search_results) {
        const std::string file_path = sr.file().file_path().string();
        if (!file_set.contains(file_path)) {
            matching_files.push_back(file_path);
        }
        file_set.emplace(file_path);
    }
    return matching_files;
}

std::vector<std::string> get_unique_matching_lines(const std::vector<SearchFileResult>& search_results) {
    std::set<std::string> line_set;
    std::vector<std::string> matching_lines;
    for (const auto& sr : search_results) {
        const std::string line = sr.line();
        if (!line_set.contains(line)) {
            matching_lines.push_back(line);
        }
        line_set.emplace(line);
    }
    sort(matching_lines.begin(), matching_lines.end());
    return matching_lines;
}

std::vector<std::string> get_all_matching_lines(const std::vector<SearchFileResult>& search_results) {
    std::vector<std::string> matching_lines;
    std::ranges::transform(search_results.begin(), search_results.end(), std::back_inserter(matching_lines),
        [](const SearchFileResult& r){ return r.line(); });
    sort(matching_lines.begin(), matching_lines.end());
    return matching_lines;
}

std::vector<std::string> get_matching_lines(const std::vector<SearchFileResult>& search_results, const bool unique) {
    return unique ? get_unique_matching_lines(search_results) : get_all_matching_lines(search_results);
}

int main(int argc, char *argv[]) {
    std::unique_ptr<SearchOptions> options;

    try {
        options = std::make_unique<SearchOptions>();
    } catch (const SearchException& e) {
        cppfind::log_msg("");
        cppfind::log_error(e.msg());
        exit(1);
    }

    try {
        auto settings = options->settings_from_args(argc, argv);

        if (settings.debug()) {
            cppfind::log_msg(settings.string());
        }

        if (settings.print_usage()) {
            options->usage();
        }

        const std::unique_ptr<SearchSettings> settings_ptr = std::make_unique<SearchSettings>(settings);

        // auto searcher = Searcher(settings);
        // auto finder = cppfind::Finder(settings_ptr);
        auto searcher = Searcher(settings_ptr);

        std::vector<SearchFileResult> results = searcher.search();

        if (settings.print_results()) {
            auto formatter = SearchResultFormatter(settings);
            std::string msg{"\nSearch results ("};
            msg.append(std::to_string(results.size())).append("):");
            cppfind::log_msg(msg);
            for (const auto& result : results) {
                cppfind::log_msg(formatter.format(result));
            }
        }

        if (settings.print_dirs()) {
            std::vector<std::string> result_dirs = get_matching_dirs(results);
            std::string msg{"\nDirectories with matches"};
            if (result_dirs.empty()) {
                msg.append(": 0");
                cppfind::log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(result_dirs.size())).append("):");
                cppfind::log_msg(msg);
                for (const auto& d : result_dirs) {
                    cppfind::log_msg(d);
                }
            }
        }

        if (settings.print_files()) {
            std::vector<std::string> result_files = get_matching_files(results);
            std::string msg{"\nFiles with matches"};
            if (result_files.empty()) {
                msg.append(": 0");
                cppfind::log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(result_files.size())).append("):");
                cppfind::log_msg(msg);
                for (const auto& f : result_files) {
                    cppfind::log_msg(f);
                }
            }
        }

        if (settings.print_lines()) {
            std::vector<std::string> result_lines = get_matching_lines(results, settings.unique_lines());
            std::string msg;
            if (settings.unique_lines()) {
                msg = "\nUnique lines with matches";
            } else {
                msg = "\nLines with matches";
            }
            if (result_lines.empty()) {
                msg.append(": 0");
                cppfind::log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(result_lines.size())).append("):");
                cppfind::log_msg(msg);
                for (const auto& l : result_lines) {
                    cppfind::log_msg(l);
                }
            }
        }
    } catch (const SearchException& e) {
        cppfind::log_msg("");
        cppfind::log_error(e.msg());
        options->usage();
    }

    return 0;
}
