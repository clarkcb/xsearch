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
        for (const auto& sr : search_results) {
            const std::string line = sr.line();
            if (!line_set.contains(line)) {
                matching_lines.push_back(line);
            }
            line_set.emplace(line);
        }
    } else {
        std::ranges::transform(search_results.begin(), search_results.end(), std::back_inserter(matching_lines),
            [](const SearchFileResult& r){ return r.line(); });
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
            const std::string match = r.line().substr(r.match_start_idx() - 1, r.match_end_idx() - 1);
            if (!match_set.contains(match)) {
                matches.push_back(match);
            }
            match_set.emplace(match);
        }
    } else {
        std::ranges::transform(search_results.begin(), search_results.end(), std::back_inserter(matches),
            [](const SearchFileResult& r){ return r.line().substr(r.match_start_idx() - 1, r.match_end_idx() - 1); });
    }
    const auto string_comparator = get_string_comparator(settings.sort_case_insensitive());
    std::ranges::sort(matches, string_comparator);
    return matches;
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

        const auto settings_ptr = std::make_unique<SearchSettings>(settings);

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
            std::string msg{"\nMatching directories"};
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
            std::string msg{"\nMatching files"};
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
        options->usage();
    }

    return 0;
}
