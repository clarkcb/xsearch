#include <memory>
#include <set>
#include "Searcher.h"
#include "SearchException.h"
#include "SearchResultFormatter.h"
#include "SearchOptions.h"
#include "cppfind.h"

using namespace cppsearch;

std::vector<cppfind::FileResult> get_matching_file_results(const std::vector<SearchFileResult>& search_results) {
    std::unordered_set<std::string> file_path_set;
    std::vector<cppfind::FileResult> matching_file_results;
    matching_file_results.reserve(search_results.size());
    for (const auto& sr : search_results) {
        const std::string file_path = sr.file().file_path().string();
        if (!file_path_set.contains(file_path)) {
            matching_file_results.push_back(sr.file());
        }
        file_path_set.emplace(file_path);
    }
    return matching_file_results;
}

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

std::function<bool(std::string&, std::string&)> get_string_comparator(const bool sort_case_insensitive) {
    if (sort_case_insensitive) {
        return [](const std::string& s1, const std::string& s2) { return strcasecmp(s1.c_str(), s2.c_str()) <= 0; };
    }
    return [](const std::string& s1, const std::string& s2) { return s1.compare(s2) <= 0; };
}

std::vector<std::string> get_matching_lines(const std::vector<SearchFileResult>& search_results, const SearchSettings& settings) {
    std::vector<std::string> matching_lines;
    if (settings.unique_lines()) {
        std::set<std::string> line_set;
        for (const auto& r : search_results) {
            if (r.line_num() > 0) {
                const std::string line = r.line();
                if (!line_set.contains(line)) {
                    matching_lines.push_back(line);
                }
                line_set.emplace(line);
            }
        }
    } else {
        for (const auto& r : search_results) {
            if (r.line_num() > 0) {
                const std::string line = r.line();
                matching_lines.push_back(line);
            }
        }
    }
    const auto string_comparator = get_string_comparator(settings.sort_case_insensitive());
    std::ranges::sort(matching_lines, string_comparator);
    return matching_lines;
}

std::vector<std::string> get_matches(const std::vector<SearchFileResult>& search_results, const SearchSettings& settings) {
    std::vector<std::string> matches;
    if (settings.unique_lines()) {
        std::set<std::string> match_set;
        for (const auto& r : search_results) {
            if (r.line_num() > 0) {
                const std::string match = r.line().substr(r.match_start_idx() - 1, r.match_end_idx() - 1);
                if (!match_set.contains(match)) {
                    matches.push_back(match);
                }
                match_set.emplace(match);
            }
        }
    } else {
        for (const auto& r : search_results) {
            if (r.line_num() > 0) {
                const std::string match = r.line().substr(r.match_start_idx() - 1, r.match_end_idx() - 1);
                matches.push_back(match);
            }
        }
    }
    const auto string_comparator = get_string_comparator(settings.sort_case_insensitive());
    std::ranges::sort(matches, string_comparator);
    return matches;
}

int main(int argc, char *argv[]) {
    const auto config = SearchConfig();
    std::unique_ptr<SearchOptions> options_ptr;
    std::unique_ptr<SearchSettings> settings_ptr;

    try {
        options_ptr = std::make_unique<SearchOptions>(config);
    } catch (const SearchException& e) {
        cppfind::log_msg("");
        cppfind::log_error(e.what());
        exit(1);
    }

    try {
        const auto settings = options_ptr->settings_from_args(argc, argv);

        if (settings.debug()) {
            cppfind::log_msg(settings.string());
        }

        if (settings.print_usage()) {
            options_ptr->usage();
        }

        settings_ptr = std::make_unique<SearchSettings>(settings);

        // auto searcher = Searcher(settings);
        // auto finder = cppfind::Finder(settings_ptr);
        const auto searcher = Searcher(config, settings_ptr);

        const std::vector<SearchFileResult> results = searcher.search();

        auto formatter = SearchResultFormatter(settings);

        if (settings.print_results()) {
            std::string msg{"\nSearch results ("};
            msg.append(std::to_string(results.size())).append("):");
            cppfind::log_msg(msg);
            for (const auto& result : results) {
                cppfind::log_msg(formatter.format(result));
            }
        }

        if (settings.print_dirs() || settings.print_files()) {
            std::vector<cppfind::FileResult> file_results = get_matching_file_results(results);

            if (settings.print_dirs()) {
                cppfind::print_file_result_dirs(file_results, formatter.file_result_formatter());
            }

            if (settings.print_files()) {
                cppfind::print_file_results(file_results, formatter.file_result_formatter());
            }
        }

        if (settings.print_lines()) {
            std::vector<std::string> result_lines = get_matching_lines(results, settings);
            std::string msg;
            if (settings.unique_lines()) {
                msg = "\nUnique matching lines";
            } else {
                msg = "\nMatching lines";
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

        if (settings.print_matches()) {
            std::vector<std::string> result_matches = get_matches(results, settings);
            std::string msg;
            if (settings.unique_lines()) {
                msg = "\nUnique matches";
            } else {
                msg = "\nMatches";
            }
            if (result_matches.empty()) {
                msg.append(": 0");
                cppfind::log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(result_matches.size())).append("):");
                cppfind::log_msg(msg);
                for (const auto& l : result_matches) {
                    cppfind::log_msg(l);
                }
            }
        }
    } catch (const SearchException& e) {
        cppfind::log_msg("");
        cppfind::log_error(e.msg());
        options_ptr->usage();
    }

    return 0;
}
